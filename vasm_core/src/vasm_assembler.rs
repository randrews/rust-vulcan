use std::collections::btree_map::BTreeMap;
use crate::ast::{Label, Scope, VASMLine};
use crate::parse_error::{AssembleError, Location};
use crate::vasm_evaluator::eval;
use crate::vasm_preprocessor::{Line, LineSource};
use std::fs;

/// This will solve all the .equ directives and return a symbol table of them.
/// .equ directives must be able to be solved in order, that is, in terms of
/// only preceding .equ directives. Anything else is an error.
fn solve_equs(lines: &[Line]) -> Result<Scope, AssembleError> {
    let mut scope: Scope = Scope::new();
    let line_nums: BTreeMap<usize, i32> = BTreeMap::new();
    for (line_num, line) in lines.iter().enumerate() {
        if let VASMLine::Equ(Label(name), expr) = &line.line {
            let value = eval(expr, line_num, &line_nums, &scope).map_err(|e| {
                AssembleError::EquResolveError(line.location.clone(), name.to_string(), e)
            })?;

            if let Some(_old_value) = scope.insert(name.clone(), value) {
                return Err(AssembleError::EquDuplicateError(
                    line.location.clone(),
                    name.to_string(),
                ));
            }
        }
    }
    Ok(scope)
}

type LineLengths = BTreeMap<usize, usize>;
type LineAddresses = BTreeMap<usize, i32>;

fn arg_length(val: i32) -> usize {
    if val < 0 {
        3
    } else if val < 256 {
        1
    } else if val < 65536 {
        2
    } else {
        3
    }
}

/// This figures out the instruction lengths. We'll do this naively; if we can't
/// immediately tell that an instruction needs only a 0/1/2 byte argument (because it's
/// a constant, or a .equ that we've solved, or something) then we'll assume it's a
/// full 24-bit argument.
///
/// - Lines that don't represent output (.equ, .org, etc) have length 0
/// - .db directives are either strings (set aside the length of the string), or
///   numbers (set aside three bytes. If it's shorter than that it still may be a variable,
///   which might grow to be larger).
/// - Opcodes with no argument are 1 byte long.
/// - Opcodes with an argument, if that argument is a constant or decidable solely with
///   what we know right now (.equs), are however long that argument is. If we don't
///   know right now (based on a label, say) then we'll set aside the full 3 bytes (so it's
///   4 bytes long, with the instruction byte).
fn measure_instructions(lines: &[Line], scope: &Scope) -> LineLengths {
    let line_nums: BTreeMap<usize, i32> = BTreeMap::new();
    let mut lengths = LineLengths::new();
    for (line_num, line) in lines.iter().enumerate() {
        match &line.line {
            VASMLine::Instruction(_, _, None) => {
                lengths.insert(line_num, 1);
            }
            VASMLine::Instruction(_, _, Some(node)) => {
                let len = eval(node, line_num, &line_nums, scope).map_or(3, arg_length);
                lengths.insert(line_num, len + 1);
            }
            VASMLine::Db(_, _) => {
                lengths.insert(line_num, 3);
            }
            VASMLine::StringDb(_, value) => {
                lengths.insert(line_num, value.len());
            }
            VASMLine::Org(_, _) | VASMLine::Equ(_, _) | VASMLine::LabelDef(_) | VASMLine::Blank => {
                lengths.insert(line_num, 0);
            }
            VASMLine::Macro(_) => unreachable!(),
        }
    }
    lengths
}

/// Time to start placing labels. The tricky part here is the .org directives, which can have
/// expressions as their arguments. We'll compromise a little bit and say that a .org directive
/// can only refer to labels that precede it, so, you can use .orgs to generate (say) a jump table
/// but still make it easy for me to figure out what refers to what.
///
/// We'll go through the lines, adding each one's length (calculated in measure_instructions) to it.
/// If it has a label, we'll store that label's new value to the scope.
///
/// But, we'll skip labels that come before .equs: that would make every .equ set to its address,
/// rather than the argument.
fn place_labels(
    lines: &[Line],
    scope: Scope,
    lengths: &LineLengths,
) -> Result<(LineAddresses, Scope), AssembleError> {
    let mut scope = scope;
    let mut address = 0;
    let mut addresses = LineAddresses::new();
    for (line_num, line) in lines.iter().enumerate() {
        if let VASMLine::Org(_, expr) = &line.line {
            address = eval(expr, line_num, &addresses, &scope)
                .map_err(|err| AssembleError::OrgResolveError(line.location.clone(), err))?;
            addresses.insert(line_num, address);
        }

        if let Some(Label(label)) = &line.line.label() {
            if !scope.contains_key(label) {
                scope.insert(label.clone(), address as i32);
            }
        }

        match &line.line {
            VASMLine::Org(_, _) => {}
            _ => {
                addresses.insert(line_num, address);
                address += *lengths.get(&line_num).unwrap_or(&0) as i32;
            }
        }
    }
    Ok((addresses, scope))
}

fn poke_word(code: &mut Vec<u8>, at: usize, word: i32) {
    let [low, mid, high, _] = word.to_le_bytes();
    code[at] = low;
    code[at + 1] = mid;
    code[at + 2] = high;
}

/// Find the lower and upper bounds where this program will place memory
fn code_bounds(
    lines: &[Line],
    line_addresses: &LineAddresses,
    line_lengths: &LineLengths,
) -> Result<(usize, usize), AssembleError> {
    let mut actual_lines = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| !(&line.line).zero_length());
    let (first_idx, _) = actual_lines.next().ok_or(AssembleError::NoCode)?;
    let start = line_addresses[&(first_idx)] as usize;

    let actual_lines = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| !(&line.line).zero_length());
    let (last_idx, _) = actual_lines.last().unwrap();
    let end = line_addresses[&(last_idx)] as usize;
    let end_length = line_lengths[&(last_idx)];

    Ok((start, end + end_length - 1))
}

/// Turn an iterable of strs into an assembled binary. This supports macros, but not
/// the `#include` macro. The resulting Vec is only as large as it needs to be; if your
/// code starts with `.org 0x400` and is five bytes long then the Vec will be five
/// bytes long and index 0 will represent 0x400.
/// ```
/// assert_eq!(
///   vasm_core::assemble_snippet(".org 0x400 \n push 5 \n add 7".lines().map(String::from)),
///   Ok(vec![0x01, 0x05, 0x05, 0x07])
/// )
/// ```
pub fn assemble_snippet<T: IntoIterator<Item = String>>(
    lines: T,
) -> Result<Vec<u8>, AssembleError> {
    let line_results: Vec<Result<Line, AssembleError>> =
        LineSource::new("<none>", lines, |_file| {
            Err(AssembleError::IncludeError(
                Location::default(),
                "Including is not supported in assembling snippets".to_string(),
            ))
        })
        .collect();

    assemble_line_results(line_results).map(|(bytes, _)| { bytes })
}

/// Assemble a file from the filesystem, opening other files as it includes them.
pub fn assemble_file(filename: &str) -> Result<(Vec<u8>, Scope), AssembleError> {
    let lines = lines_from_file(filename)?;
    let line_results: Vec<Result<Line, AssembleError>> = LineSource::new(filename, lines, |file| {
        lines_from_file(file.as_str())
    })
    .collect();

    assemble_line_results(line_results)
}

fn assemble_line_results(
    mut line_results: Vec<Result<Line, AssembleError>>,
) -> Result<(Vec<u8>, Scope), AssembleError> {
    if let Some(Err(error)) = line_results.iter().find(|line| line.is_err()) {
        Err(error.clone())
    } else {
        generate_code(line_results.iter_mut().map(|line| line.clone().unwrap()))
    }
}

fn lines_from_file(filename: &str) -> Result<Vec<String>, AssembleError> {
    let file =
        fs::read_to_string(filename).map_err(|_e| AssembleError::FileError(filename.into()))?;
    Ok(file.lines().map(String::from).collect())
}

/// At this point all lines have addresses and lengths, and all arguments are reduced to
/// numeric constants. It's time to generate code.
///
/// - Make an array of zeroes, length (end - start)
/// - Go through the list of instructions, generating code for them:
/// - .db instructions turn into byte values starting at `address - start`
/// - Opcodes turn into instruction bytes at `address - start` followed (maybe) by
///   arguments.
/// - .orgs cause us to skip ahead some in the output
///
/// The instruction bytes are formed of six bits defining the instruction followed by two
/// bits denoting how many bytes of argument follow it.
///
/// Vulcan is a little-endian architecture: multi-byte arguments / .dbs will store the
/// least-significant byte at the lowest address, then the more significant bytes following.
fn generate_code<T: IntoIterator<Item = Line>>(lines: T) -> Result<(Vec<u8>, Scope), AssembleError> {
    let lines: Vec<Line> = lines.into_iter().collect();
    let scope = solve_equs(&lines)?;
    let line_lengths = measure_instructions(&lines, &scope);
    let (line_addresses, scope) = place_labels(&lines, scope, &line_lengths)?;
    let (start, end) = code_bounds(&lines, &line_addresses, &line_lengths)?;

    let mut code = vec![0u8; end - start + 1];
    let mut current_addr = start;

    for (line_num, line) in lines.iter().enumerate() {
        match &line.line {
            VASMLine::Instruction(_, opcode, None) => {
                code[current_addr - start] = u8::from(*opcode) << 2;
                current_addr += 1;
            }
            VASMLine::Instruction(_, opcode, Some(arg)) => {
                let arg = eval(arg, line_num, &line_addresses, &scope)
                    .map_err(|err| AssembleError::ArgError(line.location.clone(), err))?;
                let len = line_lengths[&line_num] - 1;
                let instr = (u8::from(*opcode) << 2) + len as u8;
                code[current_addr - start] = instr;
                let [low, mid, high, _] = arg.to_le_bytes();
                code[current_addr - start + 1] = low;
                if len > 1 {
                    code[current_addr - start + 2] = mid
                }
                if len > 2 {
                    code[current_addr - start + 3] = high
                }
                current_addr += len + 1;
            }
            VASMLine::Db(_, arg) => {
                let arg = eval(arg, line_num, &line_addresses, &scope)
                    .map_err(|err| AssembleError::ArgError(line.location.clone(), err))?;
                poke_word(&mut code, current_addr - start, arg);
                current_addr += 3;
            }
            VASMLine::StringDb(_, string) => {
                for ch in string.as_bytes() {
                    code[current_addr - start] = *ch;
                    current_addr += 1;
                }
            }
            VASMLine::Org(_, _) => {
                current_addr = line_addresses[&(line_num + 1)] as usize;
            }
            VASMLine::Equ(_, _) | VASMLine::LabelDef(_) | VASMLine::Blank => {}
            VASMLine::Macro(_) => unreachable!(),
        }
    }

    Ok((code, scope))
}

#[cfg(test)]
mod test {
    use super::AssembleError::*;
    use super::*;
    use crate::parse_error;
    use crate::parse_error::EvalError::*;
    use crate::vasm_parser::parse_vasm_line;

    fn parse<'a, T: IntoIterator<Item = &'a str>>(lines: T) -> Vec<Line> {
        lines
            .into_iter()
            .enumerate()
            .map(|(line_num, line)| Line {
                line: parse_vasm_line(line).unwrap(),
                location: line_num.into(),
            })
            .collect()
    }

    fn place_labels_pass<'a, T: IntoIterator<Item = &'a str>>(
        lines: T,
    ) -> Result<(LineAddresses, Scope), AssembleError> {
        let parsed_lines = parse(lines);
        let scope = solve_equs(&parsed_lines).unwrap();
        let lengths = measure_instructions(&parsed_lines, &scope);
        place_labels(&parsed_lines, scope, &lengths)
    }

    fn bounds<'a, T: IntoIterator<Item = &'a str>>(
        lines: T,
    ) -> Result<(usize, usize), AssembleError> {
        let parsed_lines = parse(lines);
        let scope = solve_equs(&parsed_lines).unwrap();
        let lengths = measure_instructions(&parsed_lines, &scope);
        let (line_addresses, _scope) = place_labels(&parsed_lines, scope, &lengths)?;
        code_bounds(&parsed_lines, &line_addresses, &lengths)
    }

    #[test]
    fn test_equs() {
        assert_eq!(
            solve_equs(&parse(["blah: .equ 5+3"])),
            Ok([("blah".to_string(), 8)].into())
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ 5", "foo: .equ 3"])),
            Ok([("blah".to_string(), 5), ("foo".to_string(), 3)].into())
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ 5", "foo: .equ blah + 7"])),
            Ok([("blah".to_string(), 5), ("foo".to_string(), 12)].into())
        );
        assert_eq!(
            solve_equs(&parse(["add", "blah: .equ 5"])),
            Ok([("blah".to_string(), 5)].into())
        );
    }

    #[test]
    fn test_unsolvable_equs() {
        assert_eq!(
            solve_equs(&parse(["blah: .equ 5", "foo: .equ banana"])),
            Err(EquResolveError(
                1.into(),
                "foo".into(),
                MissingLabel("banana".into())
            ))
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ foo+3", "foo: .equ 7"])),
            Err(EquResolveError(
                0.into(),
                "blah".into(),
                MissingLabel("foo".into())
            ))
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ 3", "blah: .equ 7"])),
            Err(EquDuplicateError(1.into(), "blah".into()))
        );
    }

    #[test]
    fn test_lengths() {
        assert_eq!(
            measure_instructions(
                &parse(["add", "add 1", "add 500", "add 70000", "add -7"]),
                &[].into()
            ),
            [(0, 1), (1, 2), (2, 3), (3, 4), (4, 4)].into()
        );
        assert_eq!(
            measure_instructions(&parse([".db 7", ".db \"hello\\0\""]), &[].into()),
            [(0, 3), (1, 6)].into()
        );
        assert_eq!(
            measure_instructions(&parse([".org 256", "blah:", "foo: .equ 7"]), &[].into()),
            [(0, 0), (1, 0), (2, 0)].into()
        );
        assert_eq!(
            measure_instructions(
                &parse([".org 0x400", "push 3", "call blah", "hlt", "blah: mul 2"]),
                &[].into()
            ),
            [(0, 0), (1, 2), (2, 4), (3, 1), (4, 2)].into()
        );
        assert_eq!(
            measure_instructions(
                &parse(["add 2 + foo", "add 3 + blah", "jmpr @foo"]),
                &[("blah".to_string(), 300)].into()
            ),
            [(0, 4), (1, 3), (2, 4)].into()
        );
    }

    #[test]
    fn test_place_labels() {
        assert_eq!(
            place_labels_pass(["start: .org 256", "add", "dup"]),
            Ok((
                [(0, 256), (1, 256), (2, 257)].into(),
                [("start".to_string(), 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(["push 70000", "dup"]),
            Ok(([(0, 0), (1, 4)].into(), [].into()))
        );
        assert_eq!(
            place_labels_pass(["start: .equ 256", "blah: .org start + 4", "add"]),
            Ok((
                [(0, 0), (1, 260), (2, 260)].into(),
                [("blah".to_string(), 260), ("start".to_string(), 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(["start: .org 256", "blah: .org start + 10", "add"]),
            Ok((
                [(0, 256), (1, 266), (2, 266)].into(),
                [("blah".to_string(), 266), ("start".to_string(), 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass([
                ".org 1024",
                "nop 3",
                "call blah",
                "hlt",
                "blah: mul 2",
                "ret"
            ]),
            Ok((
                [
                    (0, 1024),
                    (1, 1024),
                    (2, 1026),
                    (3, 1030),
                    (4, 1031),
                    (5, 1033)
                ]
                .into(),
                [("blah".to_string(), 1031)].into()
            ))
        );
    }

    #[test]
    fn test_unresolvable_orgs() {
        assert_eq!(
            place_labels_pass([".org 0xffffff - blah"]),
            Err(OrgResolveError(0.into(), MissingLabel("blah".into())))
        );
        assert_eq!(
            place_labels_pass(["blah: .org blah"]),
            Err(OrgResolveError(0.into(), MissingLabel("blah".into())))
        );
    }

    #[test]
    fn test_bounds() {
        assert_eq!(bounds(["add"]), Ok((0, 0)));
        assert_eq!(bounds(["add -4"]), Ok((0, 3)));
        assert_eq!(bounds(["add 7"]), Ok((0, 1)));
        assert_eq!(bounds([".org 0x400", "add"]), Ok((1024, 1024)));
        assert_eq!(
            bounds(["start: .equ 1024", ".org start", "add"]),
            Ok((1024, 1024))
        );
        assert_eq!(
            bounds([".org 0x400", "add", ".org 0x800"]),
            Ok((1024, 1024))
        );
        assert_eq!(
            bounds([".org 0x400", "add", ".org 0x800", ".db 5", "blah:"]),
            Ok((1024, 2050))
        );
        assert_eq!(bounds([]), Err(AssembleError::NoCode));
        assert_eq!(bounds([".org 0x400"]), Err(AssembleError::NoCode));
        assert_eq!(
            bounds(["foo: .equ 3", ".org 0x400"]),
            Err(AssembleError::NoCode)
        );
    }

    #[test]
    fn test_generate_code() {
        assert_eq!(generate_code(parse(["add"])).unwrap().0, vec![4]);
        assert_eq!(
            generate_code(parse([".org 0x400", "add 7"])).unwrap().0,
            vec![5, 7]
        );
        assert_eq!(generate_code(parse([".db 57"])).unwrap().0, vec![57, 0, 0]);
        assert_eq!(generate_code(parse([".db \"AZ\0\""])).unwrap().0, vec![65, 90, 0]);
    }

    #[test]
    fn test_assemble_snippet() {
        assert_eq!(assemble_snippet(["add"].map(String::from)), Ok(vec![4]));
        assert_eq!(
            assemble_snippet(["apple"].map(String::from)),
            Err(ParseError(
                1.into(),
                parse_error::ParseError::InvalidInstruction("apple".into())
            ))
        );

        assert_eq!(
            assemble_snippet(
                ".org 0x400
                                    nop 3
                                    call blah
                                    hlt
                                    blah: mul 2
                                    ret"
                .lines()
                .map(String::from)
            ),
            Ok(vec![
                0x01, 0x03, // nop 3
                0x67, 0x07, 0x04, 0x00, // call blah (arg defaults to 3 bytes long)
                0x74, // hlt
                0x0d, 0x02, // mul 2
                0x68
            ])
        );
    }

    #[test]
    fn test_relative_blanks() {
        assert_eq!(
            assemble_snippet(
                ".org 0x400
                                    nop $+2

                                    nop 0x111111
                                    nop 0x222222"
                    .lines()
                    .map(String::from)
            ),
            Ok(vec![
                0x03, 0x08, 0x04, 0x00, 0x03, 0x11, 0x11, 0x11, 0x03, 0x22, 0x22, 0x22
            ])
        )
    }
}
