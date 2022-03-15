use std::convert::TryFrom;
use std::str::FromStr;

use pest::iterators::Pair;
use pest::Parser;

use vcore::opcodes::Opcode;

use crate::ast::{Directive, Instruction, Node, Operator, VASMLine};
use crate::parse_error::ParseError;

#[derive(Parser)]
#[grammar = "vasm.pest"]
pub struct VASMParser;

/// Perform tree-shaking on a `Node` by recursively removing single-`Node` exprs.
fn shake(node: Node) -> Node {
    match node {
        Node::Expr(first, rest) => {
            let shaken_car = shake(*first);
            if rest.is_empty() {
                shaken_car
            } else {
                let shaken_cdr = rest.into_iter().map(|(op, n)| (op, shake(n))).collect();
                Node::Expr(Box::from(shaken_car), shaken_cdr)
            }
        }
        _ => node,
    }
}

pub fn parse_vasm_line(line: &str) -> Result<VASMLine, ParseError<'_>> {
    let mut pairs = VASMParser::parse(Rule::line, line)
        .map_err(|_| ParseError::LineParseFailure)?
        .next()
        .unwrap()
        .into_inner()
        .peekable();

    let mut vasm_line = VASMLine::blank();

    if let Some(label_group) = pairs.next_if(|pair| pair.as_rule() == Rule::label_group) {
        let label = label_group.into_inner().next().unwrap();
        vasm_line = vasm_line.with_label(label.as_str())
    }

    if let Some(instruction_group) = pairs.next_if(|pair| pair.as_rule() == Rule::instruction_group)
    {
        let mut pairs = instruction_group.into_inner();
        vasm_line = vasm_line.with_instruction(Instruction::try_from(pairs.next().unwrap())?);

        if let Some(argument_group) = pairs.next() {
            vasm_line = vasm_line.with_argument(shake(Node::try_from(argument_group)?))
        }
    }

    Ok(vasm_line)
}

#[cfg(test)]
mod test {
    use vcore::opcodes::Opcode::*;

    use super::Directive::*;
    use super::*;

    impl<'a> VASMLine<'a> {
        pub fn op(opcode: Opcode) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode)
        }

        fn op_number(opcode: Opcode, argument: i32) -> VASMLine<'a> {
            Self::blank()
                .with_opcode(opcode)
                .with_argument(Node::Number(argument))
        }

        fn op_label(opcode: Opcode, argument: &'a str) -> VASMLine<'a> {
            Self::blank()
                .with_opcode(opcode)
                .with_argument(Node::Label(argument))
        }

        fn op_rel_label(opcode: Opcode, argument: &'a str) -> VASMLine<'a> {
            Self::blank()
                .with_opcode(opcode)
                .with_argument(Node::RelativeLabel(argument))
        }

        fn op_off(opcode: Opcode, argument: Node<'a>) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode).with_argument(argument)
        }

        pub fn dir_str(directive: Directive, string: &str) -> VASMLine<'a> {
            Self::blank()
                .with_directive(directive)
                .with_argument(Node::String(string.to_string()))
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse_vasm_line("add"), Ok(VASMLine::op(Add)));
        assert_eq!(parse_vasm_line("sub"), Ok(VASMLine::op(Sub)));
        assert_eq!(
            parse_vasm_line("blah"),
            Err(ParseError::InvalidInstruction("blah"))
        );
        assert_eq!(parse_vasm_line("47"), Err(ParseError::LineParseFailure));
    }

    #[test]
    fn test_numbers() {
        assert_eq!(parse_vasm_line("add 45"), Ok(VASMLine::op_number(Add, 45)));
        assert_eq!(parse_vasm_line("add 0"), Ok(VASMLine::op_number(Add, 0)));
        assert_eq!(
            parse_vasm_line("add 0x10"),
            Ok(VASMLine::op_number(Add, 16))
        );
        assert_eq!(
            parse_vasm_line("add 0b1111"),
            Ok(VASMLine::op_number(Add, 15))
        );
        assert_eq!(
            parse_vasm_line("add 0o377"),
            Ok(VASMLine::op_number(Add, 255))
        );
        assert_eq!(
            parse_vasm_line("add -17"),
            Ok(VASMLine::op_number(Add, -17))
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            parse_vasm_line(".db \"blah\""),
            Ok(VASMLine::dir_str(Db, "blah"))
        );
        assert_eq!(
            parse_vasm_line(".db \"blah\\twith escapes\\0\""),
            Ok(VASMLine::dir_str(Db, "blah\twith escapes\0"))
        )
    }

    #[test]
    fn test_exprs() {
        assert_eq!(
            parse_vasm_line("add 2 + 3 * (4 - 5) + 6"),
            Ok(VASMLine::op(Add).with_argument(Node::Expr(
                Box::from(Node::Number(2)),
                vec![
                    (
                        Operator::Add,
                        Node::Expr(
                            Box::from(Node::Number(3)),
                            vec![(
                                Operator::Mul,
                                Node::Expr(
                                    Box::from(Node::Number(4)),
                                    vec![(Operator::Sub, Node::Number(5))],
                                )
                            )],
                        )
                    ),
                    (Operator::Add, Node::Number(6))
                ],
            )))
        );
    }

    #[test]
    fn test_expr_labels() {
        assert_eq!(
            parse_vasm_line("loadw foo"),
            Ok(VASMLine::op_label(Loadw, "foo"))
        );
        assert_eq!(
            parse_vasm_line("brz @blah"),
            Ok(VASMLine::op_rel_label(Brz, "blah"))
        )
    }

    #[test]
    fn test_expr_offsets() {
        assert_eq!(
            parse_vasm_line("jmp $-2"),
            Ok(VASMLine::op_off(Jmp, Node::AbsoluteOffset(-2)))
        );
        assert_eq!(
            parse_vasm_line("brz @+3"),
            Ok(VASMLine::op_off(Brz, Node::RelativeOffset(3)))
        )
    }

    #[test]
    fn test_parse_labels() {
        assert_eq!(
            parse_vasm_line("foo: add"),
            Ok(VASMLine::op(Add).with_label("foo"))
        );
        assert_eq!(
            parse_vasm_line("bar:"),
            Ok(VASMLine::blank().with_label("bar"))
        );
        assert_eq!(
            parse_vasm_line("foo: add 43"),
            Ok(VASMLine::op_number(Add, 43).with_label("foo"))
        );
    }

    #[test]
    fn test_parse_directives() {
        assert_eq!(
            parse_vasm_line("foo: .equ"),
            Ok(VASMLine::blank().with_label("foo").with_directive(Equ))
        );
        assert_eq!(
            parse_vasm_line(".db"),
            Ok(VASMLine::blank().with_directive(Db))
        );
    }
}
