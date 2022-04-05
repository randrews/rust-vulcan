use crate::ast::{Node, Operator, Scope};
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    MissingLabel(String),
    UnknownAddress(usize),
    OffsetError(usize, i32),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::MissingLabel(label) => write!(f, "Unable to resolve label {}", label),
            EvalError::UnknownAddress(line_num) => write!(
                f,
                "Unable to calculate starting address of line {}",
                line_num
            ),
            EvalError::OffsetError(line_num, offset) => {
                write!(f, "Invalid line offset {} on line {}", offset, line_num)
            }
        }
    }
}

fn offset_line(line_num: usize, offset: i32) -> Result<usize, EvalError> {
    if (line_num as i32) + offset < 0 {
        Err(EvalError::OffsetError(line_num, offset))
    } else {
        Ok(((line_num as i32) + offset) as usize)
    }
}

/// ## Evaluating expressions
/// Now that we have a parsed file, that file has a bunch of numeric symbols in it: labels,
/// .equ directives, that sort of thing. We need to resolve all of those to constant values
/// before we can generate code. So, first part of that is being able to evaluate expressions.
///
/// This evaluates an expression in the context of a symbol table, and returns either what the
/// expression evaluates to (a number) or an error (if it references a symbol not in
/// the given symbol table, or needs to know an address that's not calculated yet).
///
/// It's a depth-first recursive traversal of the expression AST:
///
/// - If the node is a number, then it returns that number.
/// - If the node is a string, it tries to look it up in the symbol table or explodes.
/// - If the node is a relative label, it tries to look it up in the symbol table, and then
///   subtracts a given start_address. If start_address is nil (as when we're solving .equs)
///   then it errors.
/// - If the node is an expr or term, then it evaluates the children: the children are a
///   sequence of evaluate-able nodes separated by operators. So first evaluate the left-most
///   child, then use the operator to combine it with the following one, and so on.
/// - If the node is an absolute or relative line offset, it attempts to look up the start of
///   the given line in the table of line start addresses and gives that address relatively or
///   absolutely.
pub fn eval(
    node: &Node,
    line_num: usize,
    line_addresses: &BTreeMap<usize, i32>,
    scope: &Scope,
) -> Result<i32, EvalError> {
    match node {
        Node::Number(n) => Ok(*n),
        Node::Label(label) => scope.get(label.into()).map_or_else(
            || Err(EvalError::MissingLabel(label.to_string())),
            |val| Ok(*val),
        ),
        Node::RelativeLabel(label) => {
            if let Some(address) = line_addresses.get(&line_num) {
                scope.get(label.into()).map_or_else(
                    || Err(EvalError::MissingLabel(label.to_string())),
                    |val| Ok(*val - address),
                )
            } else {
                Err(EvalError::UnknownAddress(line_num))
            }
        }
        Node::AbsoluteOffset(offset) => {
            let addr = offset_line(line_num, *offset)?;
            if let Some(dest_address) = line_addresses.get(&addr) {
                Ok(*dest_address)
            } else {
                Err(EvalError::UnknownAddress(addr))
            }
        }
        Node::RelativeOffset(offset) => {
            let addr = offset_line(line_num, *offset)?;
            if let (Some(line_address), Some(dest_address)) =
                (line_addresses.get(&line_num), line_addresses.get(&addr))
            {
                Ok(*dest_address - *line_address)
            } else {
                Err(EvalError::UnknownAddress(addr))
            }
        }
        Node::Expr(car, cdr) => {
            let car = eval(car, line_num, line_addresses, scope);
            if let Ok(mut acc) = car {
                for (op, node) in cdr {
                    let rhs = eval(node, line_num, line_addresses, scope)?;
                    match op {
                        Operator::Add => acc += rhs,
                        Operator::Sub => acc -= rhs,
                        Operator::Mul => acc *= rhs,
                        Operator::Div => acc /= rhs,
                        Operator::Mod => acc %= rhs,
                    }
                }
                Ok(acc)
            } else {
                car
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::VASMLine;
    use crate::vasm_parser::parse_vasm_line;

    fn test_eval(line: &str) -> Result<i32, EvalError> {
        test_scope_addresses_eval(BTreeMap::new(), [(1, 0x400)].into(), line)
    }

    fn test_scope_eval(scope: Scope, line: &str) -> Result<i32, EvalError> {
        test_scope_addresses_eval(scope, [(1, 0x400)].into(), line)
    }

    fn test_addresses_eval(
        line_addresses: BTreeMap<usize, i32>,
        line: &str,
    ) -> Result<i32, EvalError> {
        test_scope_addresses_eval([].into(), line_addresses, line)
    }

    fn test_scope_addresses_eval(
        scope: Scope,
        line_addresses: BTreeMap<usize, i32>,
        line: &str,
    ) -> Result<i32, EvalError> {
        if let Ok(VASMLine::Instruction(_, _, Some(arg))) = parse_vasm_line(line) {
            eval(&arg, 1, &line_addresses, &scope)
        } else {
            panic!("Failed to parse an instruction line with an argument")
        }
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(test_eval("add 4"), Ok(4));
        assert_eq!(test_eval("add 2 + 3"), Ok(5));
        assert_eq!(test_eval("add 6 - 3 - 1"), Ok(2));
        assert_eq!(test_eval("add 6 - (3 - 1)"), Ok(4));
        assert_eq!(test_eval("add 6 / (3-1) * 7"), Ok(21));
        assert_eq!(test_eval("add (1+2+4) % 5"), Ok(2));
    }

    #[test]
    fn test_labels() {
        assert_eq!(
            test_scope_eval([("apple".into(), 5)].into(), "add apple"),
            Ok(5)
        );
        assert_eq!(
            test_scope_eval([("apple".into(), 5)].into(), "add apple + 7"),
            Ok(12)
        );
        assert_eq!(
            test_scope_eval([("apple".into(), 5)].into(), "add 5 + apple"),
            Ok(10)
        );
        assert_eq!(
            test_scope_eval(
                [("apple".into(), 5), ("banana".into(), 3)].into(),
                "add apple * banana"
            ),
            Ok(15)
        );
        assert_eq!(
            test_scope_eval([("apple".into(), 5)].into(), "add banana"),
            Err(EvalError::MissingLabel("banana".into()))
        );
        assert_eq!(
            test_scope_eval([].into(), "add apple + 2"),
            Err(EvalError::MissingLabel("apple".into()))
        );
        assert_eq!(
            test_scope_eval([].into(), "add 2 + apple"),
            Err(EvalError::MissingLabel("apple".into()))
        );
    }

    #[test]
    fn test_relative_labels() {
        assert_eq!(
            test_scope_eval([("apple".into(), 0x500)].into(), "jmpr @apple"),
            Ok(0x100)
        );
        assert_eq!(
            test_scope_eval([("apple".into(), 0x300)].into(), "jmpr @apple"),
            Ok(-256)
        );
        assert_eq!(
            test_scope_addresses_eval([("apple".into(), 0x300)].into(), [].into(), "jmpr @apple"),
            Err(EvalError::UnknownAddress(1))
        );
    }

    #[test]
    fn test_absolute_offset() {
        assert_eq!(
            test_addresses_eval([(4, 0x410)].into(), "jmp $+3"),
            Ok(0x410)
        );
        assert_eq!(
            test_addresses_eval([(4, 0x410)].into(), "jmp $+1"),
            Err(EvalError::UnknownAddress(2))
        );
    }

    #[test]
    fn test_relative_offset() {
        assert_eq!(
            test_addresses_eval([(1, 0x400), (4, 0x410)].into(), "brz @+3"),
            Ok(0x10)
        );
        assert_eq!(
            test_addresses_eval([(4, 0x410)].into(), "brz @+2"),
            Err(EvalError::UnknownAddress(3))
        );
        assert_eq!(
            test_addresses_eval([(1, 0x400)].into(), "brz @+7"),
            Err(EvalError::UnknownAddress(8))
        );
    }
}
