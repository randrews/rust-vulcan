use crate::ast::{Node, Operator};
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalError<'a> {
    MissingLabel(&'a str),
    UnknownAddress(i32),
}

impl<'a> Display for EvalError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::MissingLabel(label) => write!(f, "Unable to resolve label {}", label),
            EvalError::UnknownAddress(line_num) => write!(
                f,
                "Unable to calculate starting address of line {}",
                line_num
            ),
        }
    }
}

pub fn eval<'a>(
    node: Node<'a>,
    line_num: i32,
    line_addresses: &BTreeMap<i32, i32>,
    scope: &BTreeMap<&'a str, i32>,
) -> Result<i32, EvalError<'a>> {
    match node {
        Node::Number(n) => Ok(n),
        Node::Label(label) => scope
            .get(label)
            .map_or_else(|| Err(EvalError::MissingLabel(label)), |val| Ok(*val)),
        Node::RelativeLabel(label) => {
            if let Some(address) = line_addresses.get(&line_num) {
                scope.get(label).map_or_else(
                    || Err(EvalError::MissingLabel(label)),
                    |val| Ok(*val - address),
                )
            } else {
                Err(EvalError::UnknownAddress(line_num))
            }
        }
        Node::AbsoluteOffset(offset) => {
            if let Some(dest_address) = line_addresses.get(&(line_num + offset)) {
                Ok(*dest_address)
            } else {
                Err(EvalError::UnknownAddress(line_num + offset))
            }
        }
        Node::RelativeOffset(offset) => {
            if let (Some(line_address), Some(dest_address)) = (
                line_addresses.get(&line_num),
                line_addresses.get(&(line_num + offset)),
            ) {
                Ok(*dest_address - *line_address)
            } else {
                Err(EvalError::UnknownAddress(line_num + offset))
            }
        }
        Node::Expr(car, cdr) => {
            let car = eval(*car, line_num, line_addresses, scope);
            if let Ok(mut acc) = car {
                for (op, node) in cdr {
                    if let Ok(rhs) = eval(node, line_num, line_addresses, scope) {
                        match op {
                            Operator::Add => acc += rhs,
                            Operator::Sub => acc -= rhs,
                            Operator::Mul => acc *= rhs,
                            Operator::Div => acc /= rhs,
                            Operator::Mod => acc %= rhs,
                        }
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

    fn test_scope_eval<'a>(
        scope: BTreeMap<&'a str, i32>,
        line: &'a str,
    ) -> Result<i32, EvalError<'a>> {
        test_scope_addresses_eval(scope, [(1, 0x400)].into(), line)
    }

    fn test_addresses_eval(
        line_addresses: BTreeMap<i32, i32>,
        line: &str,
    ) -> Result<i32, EvalError> {
        test_scope_addresses_eval([].into(), line_addresses, line)
    }

    fn test_scope_addresses_eval<'a>(
        scope: BTreeMap<&'a str, i32>,
        line_addresses: BTreeMap<i32, i32>,
        line: &'a str,
    ) -> Result<i32, EvalError<'a>> {
        if let Ok(VASMLine::Instruction(_, _, Some(arg))) = parse_vasm_line(line) {
            eval(arg, 1, &line_addresses, &scope)
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
        assert_eq!(test_scope_eval([("apple", 5)].into(), "add apple"), Ok(5));
        assert_eq!(
            test_scope_eval([("apple", 5)].into(), "add apple + 7"),
            Ok(12)
        );
        assert_eq!(
            test_scope_eval([("apple", 5), ("banana", 3)].into(), "add apple * banana"),
            Ok(15)
        );
        assert_eq!(
            test_scope_eval([("apple", 5)].into(), "add banana"),
            Err(EvalError::MissingLabel("banana"))
        );
    }

    #[test]
    fn test_relative_labels() {
        assert_eq!(
            test_scope_eval([("apple", 0x500)].into(), "jmpr @apple"),
            Ok(0x100)
        );
        assert_eq!(
            test_scope_eval([("apple", 0x300)].into(), "jmpr @apple"),
            Ok(-256)
        );
        assert_eq!(
            test_scope_addresses_eval([("apple", 0x300)].into(), [].into(), "jmpr @apple"),
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
