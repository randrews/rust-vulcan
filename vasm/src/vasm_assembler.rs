use crate::ast::{Label, Macro, Scope, VASMLine};
use crate::parse_error::ParseError;
use crate::vasm_evaluator::{eval, EvalError};
use crate::vasm_parser::parse_vasm_line;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum AssembleError {
    ParseError(usize, ParseError),
    EquResolveError(usize, String, EvalError),
    EquDuplicateError(usize, String),
    OrgResolveError(usize, EvalError),
    ArgError(usize, EvalError),
    NoCode,
    IncludeError(usize, String),
    MacroError(usize),
}

impl Display for AssembleError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AssembleError::EquResolveError(line, name, err) => {
                write!(f, "Cannot resolve .equ {} on line {}: {}", name, line, err)
            }
            AssembleError::EquDuplicateError(line, name) => {
                write!(f, "Duplicate .equ {} on line {}", name, line)
            }
            AssembleError::OrgResolveError(line, err) => {
                write!(f, "Cannot resolve .org on line {}: {}", line, err)
            }
            AssembleError::ArgError(line, err) => {
                write!(f, "Cannot calculate argument on line {}: {}", line, err)
            }
            AssembleError::NoCode => {
                write!(f, "No output would be generated by this code")
            }
            AssembleError::ParseError(line, err) => {
                write!(f, "Parse error on line {}: {}", line, err)
            }
            AssembleError::IncludeError(line, file) => {
                write!(f, "Cannot read \"{}\" on line {}", file, line)
            }
            AssembleError::MacroError(line) => {
                write!(f, "Malformed macro control structure on line {}", line)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Line {
    line: VASMLine,
    line_num: usize,
    file: String,
}

#[derive(Debug, Clone, PartialEq)]
enum LoopType {
    While,
    Until,
}

impl From<Macro> for LoopType {
    fn from(mac: Macro) -> Self {
        match mac {
            Macro::While => LoopType::While,
            Macro::Until => LoopType::Until,
            _ => unreachable!()
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
enum ControlStructure {
    Target(String),
    Loop(String, LoopType),
}

fn preprocess<'a, T, F>(iter: T, filename: String, include: &F) -> Result<Vec<Line>, AssembleError>
where
    T: IntoIterator<Item = &'a str>,
    F: Fn(String) -> Result<T, AssembleError>,
{
    let mut current_sym = 0;
    let mut gensym = || {
        current_sym += 1;
        format!("__gensym_{}", current_sym)
    };
    let mut all_lines = Vec::new();

    let mut iter_stack = vec![iter.into_iter().enumerate()];
    let mut filename_stack = vec![filename];
    let mut control_stack = Vec::new();

    while !iter_stack.is_empty() {
        if let Some((line_idx, line)) = iter_stack.last_mut().unwrap().next() {
            match parse_vasm_line(line)
                .map_err(|err| AssembleError::ParseError(line_idx + 1, err))?
            {
                VASMLine::Macro(mac) => match mac {
                    Macro::Include(file) => {
                        filename_stack.push(file.clone());
                        iter_stack.push(include(file)?.into_iter().enumerate());
                    }
                    Macro::If => {
                        let label = gensym();
                        control_stack.push(ControlStructure::Target(label.clone()));
                        all_lines.push(Line {
                            line: parse_vasm_line(format!("brz @{}", label).as_str()).unwrap(),
                            line_num: line_idx + 1,
                            file: filename_stack.last().unwrap().clone(),
                        })
                    }
                    Macro::Unless => {
                        let label = gensym();
                        control_stack.push(ControlStructure::Target(label.clone()));
                        all_lines.push(Line {
                            line: parse_vasm_line(format!("brnz @{}", label).as_str()).unwrap(),
                            line_num: line_idx + 1,
                            file: filename_stack.last().unwrap().clone(),
                        })
                    }
                    Macro::Else => {
                        if let Some(ControlStructure::Target(old_end)) = control_stack.pop() {
                            let new_end = gensym();
                            control_stack.push(ControlStructure::Target(new_end.clone()));
                            all_lines.push(Line {
                                line: parse_vasm_line(format!("jmpr @{}", new_end).as_str())
                                    .unwrap(),
                                line_num: line_idx + 1,
                                file: filename_stack.last().unwrap().clone(),
                            });
                            all_lines.push(Line {
                                line: VASMLine::LabelDef(Label(old_end)),
                                line_num: line_idx + 1,
                                file: filename_stack.last().unwrap().clone(),
                            })
                        } else {
                            return Err(AssembleError::MacroError(line_idx + 1));
                        }
                    }
                    Macro::While |
                    Macro::Until => {
                        let label = gensym();
                        control_stack.push(ControlStructure::Loop(label.clone(), mac.into()));
                        all_lines.push(Line {
                            line: VASMLine::LabelDef(Label(label)),
                            line_num: line_idx + 1,
                            file: filename_stack.last().unwrap().clone(),
                        })
                    }
                    Macro::Do => {}
                    Macro::End => {}
                },
                normal_line => all_lines.push(Line {
                    line: normal_line,
                    line_num: line_idx + 1,
                    file: filename_stack.last().unwrap().clone(),
                }),
            }
        } else {
            iter_stack.pop();
            filename_stack.pop();
        }
    }

    Ok(all_lines)
}

/// This will solve all the .equ directives and return a symbol table of them.
/// .equ directives must be able to be solved in order, that is, in terms of
/// only preceding .equ directives. Anything else is an error.
fn solve_equs(lines: &[VASMLine]) -> Result<Scope, AssembleError> {
    let mut scope: Scope = Scope::new();
    let line_nums: BTreeMap<usize, i32> = BTreeMap::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = line_idx + 1;
        if let VASMLine::Equ(Label(name), expr) = line {
            let value = eval(expr, line_num, &line_nums, &scope)
                .map_err(|e| AssembleError::EquResolveError(line_num, name.to_string(), e))?;

            if let Some(_old_value) = scope.insert(name.clone(), value) {
                return Err(AssembleError::EquDuplicateError(line_num, name.to_string()));
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
fn measure_instructions(lines: &[VASMLine], scope: &Scope) -> LineLengths {
    let line_nums: BTreeMap<usize, i32> = BTreeMap::new();
    let mut lengths = LineLengths::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = line_idx + 1;
        match line {
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
            VASMLine::Org(_, _) | VASMLine::Equ(_, _) | VASMLine::LabelDef(_) => {
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
    lines: &[VASMLine],
    scope: Scope,
    lengths: &LineLengths,
) -> Result<(LineAddresses, Scope), AssembleError> {
    let mut scope = scope;
    let mut address = 0;
    let mut addresses = LineAddresses::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = line_idx + 1;

        if let VASMLine::Org(_, expr) = line {
            address = eval(expr, line_num, &addresses, &scope)
                .map_err(|err| AssembleError::OrgResolveError(line_num, err))?;
            addresses.insert(line_num, address);
        }

        if let Some(Label(label)) = line.label() {
            if !scope.contains_key(label) {
                scope.insert(label.clone(), address as i32);
            }
        }

        match line {
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
    lines: &[VASMLine],
    line_addresses: &LineAddresses,
    line_lengths: &LineLengths,
) -> Result<(usize, usize), AssembleError> {
    let mut actual_lines = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| !line.zero_length());
    let (first_idx, _) = actual_lines.next().ok_or(AssembleError::NoCode)?;
    let start = line_addresses[&(first_idx + 1)] as usize;

    let actual_lines = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| !line.zero_length());
    let (last_idx, _) = actual_lines.last().unwrap();
    let end = line_addresses[&(last_idx + 1)] as usize;
    let end_length = line_lengths[&(last_idx + 1)];

    Ok((start, end + end_length - 1))
}

pub fn assemble<'a, T: IntoIterator<Item = &'a str>>(lines: T) -> Result<Vec<u8>, AssembleError> {
    let mut parsed = Vec::new();
    for (line_idx, line) in lines.into_iter().enumerate() {
        let line_num = line_idx + 1;
        parsed.push(parse_vasm_line(line).map_err(|err| AssembleError::ParseError(line_num, err))?)
    }

    generate_code(parsed)
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
fn generate_code(lines: Vec<VASMLine>) -> Result<Vec<u8>, AssembleError> {
    let lines: Vec<VASMLine> = lines.into_iter().collect();
    let scope = solve_equs(&lines)?;
    let line_lengths = measure_instructions(&lines, &scope);
    let (line_addresses, scope) = place_labels(&lines, scope, &line_lengths)?;
    let (start, end) = code_bounds(&lines, &line_addresses, &line_lengths)?;

    let mut code = vec![0u8; end - start + 1];
    let mut current_addr = start;

    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = line_idx + 1;
        match line {
            VASMLine::Instruction(_, opcode, None) => {
                code[current_addr] = u8::from(*opcode) << 2;
                current_addr += 1;
            }
            VASMLine::Instruction(_, opcode, Some(arg)) => {
                let arg = eval(arg, line_num, &line_addresses, &scope)
                    .map_err(|err| AssembleError::ArgError(line_num, err))?;
                let len = arg_length(arg);
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
                    .map_err(|err| AssembleError::ArgError(line_num, err))?;
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
            VASMLine::Equ(_, _) | VASMLine::LabelDef(_) => {}
            VASMLine::Macro(_) => unreachable!(),
        }
    }

    Ok(code)
}

#[cfg(test)]
mod test {
    use super::AssembleError::*;
    use super::EvalError::*;
    use super::*;
    use crate::ast::{Node, VASMLine};
    use crate::parse_error;
    use crate::vasm_parser::parse_vasm_line;
    use vcore::opcodes::Opcode;

    fn parse<'a, T: IntoIterator<Item = &'a str>>(lines: T) -> Vec<VASMLine> {
        lines
            .into_iter()
            .map(|line| parse_vasm_line(line).unwrap())
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
                2,
                "foo".into(),
                MissingLabel("banana".into())
            ))
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ foo+3", "foo: .equ 7"])),
            Err(EquResolveError(
                1,
                "blah".into(),
                MissingLabel("foo".into())
            ))
        );
        assert_eq!(
            solve_equs(&parse(["blah: .equ 3", "blah: .equ 7"])),
            Err(EquDuplicateError(2, "blah".into()))
        );
    }

    #[test]
    fn test_lengths() {
        assert_eq!(
            measure_instructions(
                &parse(["add", "add 1", "add 500", "add 70000", "add -7"]),
                &[].into()
            ),
            [(1, 1), (2, 2), (3, 3), (4, 4), (5, 4)].into()
        );
        assert_eq!(
            measure_instructions(&parse([".db 7", ".db \"hello\\0\""]), &[].into()),
            [(1, 3), (2, 6)].into()
        );
        assert_eq!(
            measure_instructions(&parse([".org 256", "blah:", "foo: .equ 7"]), &[].into()),
            [(1, 0), (2, 0), (3, 0)].into()
        );
        assert_eq!(
            measure_instructions(
                &parse(["add 2 + foo", "add 3 + blah", "jmpr @foo"]),
                &[("blah".to_string(), 300)].into()
            ),
            [(1, 4), (2, 3), (3, 4)].into()
        );
    }

    #[test]
    fn test_place_labels() {
        assert_eq!(
            place_labels_pass(["start: .org 256", "add", "dup"]),
            Ok((
                [(1, 256), (2, 256), (3, 257)].into(),
                [("start".to_string(), 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(["push 70000", "dup"]),
            Ok(([(1, 0), (2, 4)].into(), [].into()))
        );
        assert_eq!(
            place_labels_pass(["start: .equ 256", "blah: .org start + 4", "add"]),
            Ok((
                [(1, 0), (2, 260), (3, 260)].into(),
                [("blah".to_string(), 260), ("start".to_string(), 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(["start: .org 256", "blah: .org start + 10", "add"]),
            Ok((
                [(1, 256), (2, 266), (3, 266)].into(),
                [("blah".to_string(), 266), ("start".to_string(), 256)].into()
            ))
        );
    }

    #[test]
    fn test_unresolvable_orgs() {
        assert_eq!(
            place_labels_pass([".org 0xffffff - blah"]),
            Err(OrgResolveError(1, EvalError::MissingLabel("blah".into())))
        );
        assert_eq!(
            place_labels_pass(["blah: .org blah"]),
            Err(OrgResolveError(1, EvalError::MissingLabel("blah".into())))
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
        assert_eq!(generate_code(parse(["add"])), Ok(vec![4]));
        assert_eq!(
            generate_code(parse([".org 0x400", "add 7"])),
            Ok(vec![5, 7])
        );
        assert_eq!(generate_code(parse([".db 57"])), Ok(vec![57, 0, 0]));
        assert_eq!(generate_code(parse([".db \"AZ\0\""])), Ok(vec![65, 90, 0]));
    }

    #[test]
    fn test_assemble() {
        assert_eq!(assemble(["add"]), Ok(vec![4]));
        assert_eq!(
            assemble(["apple"]),
            Err(ParseError(
                1,
                parse_error::ParseError::InvalidInstruction("apple".into())
            ))
        );
    }

    #[test]
    fn test_preprocess() {
        let include = |name: String| Err(AssembleError::IncludeError(1, name));
        let lines = vec!["add"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![Line {
                line_num: 1,
                file: "blah".to_string(),
                line: VASMLine::Instruction(None, Opcode::Add, None)
            }])
        );
    }

    #[test]
    fn test_preprocess_include() {
        let include = |_name: String| Ok(vec!["sub"]);
        let lines = vec!["#include \"foo\"", "add"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![
                Line {
                    line_num: 1,
                    file: "foo".to_string(),
                    line: VASMLine::Instruction(None, Opcode::Sub, None)
                },
                Line {
                    line_num: 2,
                    file: "blah".to_string(),
                    line: VASMLine::Instruction(None, Opcode::Add, None)
                }
            ])
        );
    }

    #[test]
    fn test_preprocess_if() {
        let include = |_name: String| Ok(vec![]);
        let lines = vec!["#if"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![Line {
                line_num: 1,
                file: "blah".to_string(),
                line: VASMLine::Instruction(
                    None,
                    Opcode::Brz,
                    Some(Node::relative_label("__gensym_1"))
                )
            }])
        )
    }

    #[test]
    fn test_preprocess_else() {
        let include = |_name: String| Ok(vec![]);
        let lines = vec!["#if", "#else"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![
                Line {
                    line_num: 1,
                    file: "blah".to_string(),
                    line: VASMLine::Instruction(
                        None,
                        Opcode::Brz,
                        Some(Node::relative_label("__gensym_1"))
                    )
                },
                Line {
                    line_num: 2,
                    file: "blah".to_string(),
                    line: VASMLine::Instruction(
                        None,
                        Opcode::Jmpr,
                        Some(Node::relative_label("__gensym_2"))
                    )
                },
                Line {
                    line_num: 2,
                    file: "blah".to_string(),
                    line: VASMLine::LabelDef(Label("__gensym_1".to_string()))
                }
            ])
        )
    }

    #[test]
    fn test_preprocess_while() {
        let include = |_name: String| Ok(vec![]);
        let lines = vec!["#while"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![
                Line {
                    line_num: 1,
                    file: "blah".to_string(),
                    line: VASMLine::LabelDef(Label("__gensym_1".to_string()))
                }
            ])
        )
    }

    #[test]
    fn test_preprocess_until() {
        let include = |_name: String| Ok(vec![]);
        let lines = vec!["#until"];
        assert_eq!(
            preprocess(lines, "blah".to_string(), &include),
            Ok(vec![
                Line {
                    line_num: 1,
                    file: "blah".to_string(),
                    line: VASMLine::LabelDef(Label("__gensym_1".to_string()))
                }
            ])
        )
    }
}
