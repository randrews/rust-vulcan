use crate::ast::{Label, VASMLine};
use crate::vasm_evaluator::{eval, EvalError, Scope};
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum AssembleError<'a> {
    EquResolveError(i32, &'a str, EvalError<'a>),
    EquDuplicateError(i32, &'a str),
    OrgResolveError(i32, EvalError<'a>),
}

impl<'a> Display for AssembleError<'a> {
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
        }
    }
}

/// This will solve all the .equ directives and return a symbol table of them.
/// .equ directives must be able to be solved in order, that is, in terms of
/// only preceding .equ directives. Anything else is an error.
pub fn solve_equs<'a>(lines: &[VASMLine<'a>]) -> Result<Scope<'a>, AssembleError<'a>> {
    let mut scope: Scope = Scope::new();
    let line_nums: BTreeMap<i32, i32> = BTreeMap::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = (line_idx + 1) as i32;
        if let VASMLine::Equ(Label(name), expr) = line {
            let value = eval(expr, line_num as i32, &line_nums, &scope)
                .map_err(|e| AssembleError::EquResolveError(line_num as i32, name, e))?;

            if let Some(_old_value) = scope.insert(name, value) {
                return Err(AssembleError::EquDuplicateError(line_num as i32, name));
            }
        }
    }
    Ok(scope)
}

type LineLengths = BTreeMap<i32, usize>;
type LineAddresses = BTreeMap<i32, i32>;

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
pub fn measure_instructions<'a>(lines: &[VASMLine<'a>], scope: &Scope) -> LineLengths {
    let line_nums: BTreeMap<i32, i32> = BTreeMap::new();
    let mut lengths = LineLengths::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = (line_idx + 1) as i32;
        match line {
            VASMLine::Instruction(_, _, None) => {
                lengths.insert(line_num, 1);
            }
            VASMLine::Instruction(_, _, Some(node)) => {
                let len = eval(node, line_num as i32, &line_nums, scope).map_or(3, arg_length);
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
fn place_labels<'a>(
    lines: &[VASMLine<'a>],
    scope: Scope<'a>,
    lengths: &LineLengths,
) -> Result<(LineAddresses, Scope<'a>), AssembleError<'a>> {
    let mut scope = scope;
    let mut address = 0;
    let mut addresses = LineAddresses::new();
    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = (line_idx + 1) as i32;

        if let VASMLine::Org(_, expr) = line {
            address = eval(expr, line_num, &addresses, &scope)
                .map_err(|err| AssembleError::OrgResolveError(line_num, err))?;
            addresses.insert(line_num, address);
        }

        if let Some(Label(label)) = line.label() {
            if !scope.contains_key(label) {
                scope.insert(label, address as i32);
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

#[cfg(test)]
mod test {
    use super::AssembleError::*;
    use super::EvalError::*;
    use super::*;
    use crate::ast::VASMLine;
    use crate::vasm_parser::parse_vasm_line;

    fn parse<'a>(lines: &'a [&str]) -> Vec<VASMLine<'a>> {
        lines
            .iter()
            .map(|line| parse_vasm_line(*line).unwrap())
            .collect()
    }

    fn place_labels_pass<'a>(
        lines: &'a [&str],
    ) -> Result<(LineAddresses, Scope<'a>), AssembleError<'a>> {
        let lines = parse(lines);
        let scope = solve_equs(&lines).unwrap();
        let lengths = measure_instructions(&lines, &scope);
        place_labels(&lines, scope, &lengths)
    }

    #[test]
    fn test_equs() {
        assert_eq!(
            solve_equs(&parse(&["blah: .equ 5+3"])),
            Ok([("blah", 8)].into())
        );
        assert_eq!(
            solve_equs(&parse(&["blah: .equ 5", "foo: .equ 3"])),
            Ok([("blah", 5), ("foo", 3)].into())
        );
        assert_eq!(
            solve_equs(&parse(&["blah: .equ 5", "foo: .equ blah + 7"])),
            Ok([("blah", 5), ("foo", 12)].into())
        );
        assert_eq!(
            solve_equs(&parse(&["add", "blah: .equ 5"])),
            Ok([("blah", 5)].into())
        );
    }

    #[test]
    fn test_unsolvable_equs() {
        assert_eq!(
            solve_equs(&parse(&["blah: .equ 5", "foo: .equ banana"])),
            Err(EquResolveError(2, "foo", MissingLabel("banana")))
        );
        assert_eq!(
            solve_equs(&parse(&["blah: .equ foo+3", "foo: .equ 7"])),
            Err(EquResolveError(1, "blah", MissingLabel("foo")))
        );
        assert_eq!(
            solve_equs(&parse(&["blah: .equ 3", "blah: .equ 7"])),
            Err(EquDuplicateError(2, "blah"))
        );
    }

    #[test]
    fn test_lengths() {
        assert_eq!(
            measure_instructions(
                &parse(&["add", "add 1", "add 500", "add 70000", "add -7"]),
                &[].into()
            ),
            [(1, 1), (2, 2), (3, 3), (4, 4), (5, 4)].into()
        );
        assert_eq!(
            measure_instructions(&parse(&[".db 7", ".db \"hello\\0\""]), &[].into()),
            [(1, 3), (2, 6)].into()
        );
        assert_eq!(
            measure_instructions(&parse(&[".org 256", "blah:", "foo: .equ 7"]), &[].into()),
            [(1, 0), (2, 0), (3, 0)].into()
        );
        assert_eq!(
            measure_instructions(
                &parse(&["add 2 + foo", "add 3 + blah", "jmpr @foo"]),
                &[("blah", 300)].into()
            ),
            [(1, 4), (2, 3), (3, 4)].into()
        );
    }

    #[test]
    fn test_place_labels() {
        assert_eq!(
            place_labels_pass(&["start: .org 256", "add", "dup"]),
            Ok((
                [(1, 256), (2, 256), (3, 257)].into(),
                [("start", 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(&["push 70000", "dup"]),
            Ok(([(1, 0), (2, 4)].into(), [].into()))
        );
        assert_eq!(
            place_labels_pass(&["start: .equ 256", "blah: .org start + 4", "add"]),
            Ok((
                [(1, 0), (2, 260), (3, 260)].into(),
                [("blah", 260), ("start", 256)].into()
            ))
        );
        assert_eq!(
            place_labels_pass(&["start: .org 256", "blah: .org start + 10", "add"]),
            Ok((
                [(1, 256), (2, 266), (3, 266)].into(),
                [("blah", 266), ("start", 256)].into()
            ))
        );
    }

    #[test]
    fn test_unresolvable_orgs() {
        assert_eq!(
            place_labels_pass(&[".org 0xffffff - blah"]),
            Err(OrgResolveError(1, EvalError::MissingLabel("blah")))
        );
        assert_eq!(
            place_labels_pass(&["blah: .org blah"]),
            Err(OrgResolveError(1, EvalError::MissingLabel("blah")))
        );
    }
}
