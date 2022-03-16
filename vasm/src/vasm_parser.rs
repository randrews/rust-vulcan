use std::convert::TryFrom;

use pest::Parser;

use vcore::opcodes::Opcode;

use crate::ast::{Label, Node, VASMLine};
use crate::parse_error::ParseError;

#[derive(Parser)]
#[grammar = "vasm.pest"]
pub struct VASMParser;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

trait Children {
    fn first(self) -> Self;
    fn only(self) -> Self;
}

impl<'a> Children for Pair<'a> {
    fn first(self) -> Self {
        self.into_inner().next().unwrap()
    }

    fn only(self) -> Pair<'a> {
        let mut iter = self.into_inner();
        let child = iter.next().unwrap();
        debug_assert_eq!(iter.next(), None);
        child
    }
}

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

fn optional_label(pair: Option<Pair>) -> Option<Label> {
    pair.map(|label| Label(label.only().as_str()))
}

pub fn parse_vasm_line(line: &str) -> Result<VASMLine, ParseError<'_>> {
    let line = VASMParser::parse(Rule::line, line)
        .map_err(|_| ParseError::LineParseFailure)?
        .next()
        .unwrap()
        .first();

    match line.as_rule() {
        Rule::instruction => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let opcode = Opcode::try_from(
                iter.next_if(|pair| pair.as_rule() == Rule::opcode)
                    .unwrap()
                    .as_str(),
            )?;
            let argument = iter
                .next_if(|pair| pair.as_rule() == Rule::expr)
                .map(|expr| shake(Node::from(expr)));
            return Ok(VASMLine::Instruction(label, opcode, argument));
        }
        Rule::db_directive => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let argument = shake(Node::from(iter.next().unwrap()));
            return Ok(VASMLine::Db(label, argument));
        }
        Rule::org_directive => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let argument = shake(Node::from(iter.next().unwrap()));
            return Ok(VASMLine::Org(label, argument));
        }
        Rule::equ_directive => {
            let mut iter = line.into_inner();
            let label = Label(iter.next().unwrap().only().as_str());
            let argument = shake(Node::from(iter.next().unwrap()));
            return Ok(VASMLine::Equ(label, argument));
        }
        Rule::label_def => {
            return Ok(VASMLine::LabelDef(Label(line.only().as_str())));
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use vcore::opcodes::Opcode::*;

    use super::*;
    use crate::ast::Operator;

    fn number(number: i32) -> Node<'static> {
        Node::Number(number)
    }

    fn string(s: &str) -> Node<'static> {
        Node::String(s.to_string())
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse_vasm_line("add"),
            Ok(VASMLine::Instruction(None, Add, None))
        );
        assert_eq!(
            parse_vasm_line("sub"),
            Ok(VASMLine::Instruction(None, Sub, None))
        );
        assert_eq!(
            parse_vasm_line("blah"),
            Err(ParseError::InvalidInstruction("blah"))
        );
        assert_eq!(parse_vasm_line("47"), Err(ParseError::LineParseFailure));
    }

    #[test]
    fn test_numbers() {
        assert_eq!(
            parse_vasm_line("add 45"),
            Ok(VASMLine::Instruction(None, Add, Some(number(45))))
        );
        assert_eq!(
            parse_vasm_line("blah: add 45"),
            Ok(VASMLine::Instruction(
                Some(Label("blah")),
                Add,
                Some(number(45))
            ))
        );
        assert_eq!(
            parse_vasm_line("add 0"),
            Ok(VASMLine::Instruction(None, Add, Some(number(0))))
        );
        assert_eq!(
            parse_vasm_line("add 0x10"),
            Ok(VASMLine::Instruction(None, Add, Some(number(16))))
        );
        assert_eq!(
            parse_vasm_line("add 0b1111"),
            Ok(VASMLine::Instruction(None, Add, Some(number(15))))
        );
        assert_eq!(
            parse_vasm_line("add 0o377"),
            Ok(VASMLine::Instruction(None, Add, Some(number(255))))
        );
        assert_eq!(
            parse_vasm_line("add -17"),
            Ok(VASMLine::Instruction(None, Add, Some(number(-17))))
        );
    }

    #[test]
    fn test_dbs() {
        assert_eq!(
            parse_vasm_line(".db \"blah\""),
            Ok(VASMLine::Db(None, string("blah")))
        );
        assert_eq!(
            parse_vasm_line(".db \"blah\\twith escapes\\0\""),
            Ok(VASMLine::Db(None, string("blah\twith escapes\0")))
        );
        assert_eq!(
            parse_vasm_line("foo: .db 47"),
            Ok(VASMLine::Db(Some(Label("foo")), number(47)))
        );
    }

    #[test]
    fn test_exprs() {
        assert_eq!(
            parse_vasm_line("add 2 + 3 * (4 - 5) + 6"),
            Ok(VASMLine::Instruction(
                None,
                Add,
                Some(Node::Expr(
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
                ))
            ))
        );
    }

    #[test]
    fn test_expr_labels() {
        assert_eq!(
            parse_vasm_line("loadw foo"),
            Ok(VASMLine::Instruction(None, Loadw, Some(Node::Label("foo"))))
        );
        assert_eq!(
            parse_vasm_line("brz @blah"),
            Ok(VASMLine::Instruction(
                None,
                Brz,
                Some(Node::RelativeLabel("blah"))
            ))
        )
    }

    #[test]
    fn test_expr_offsets() {
        assert_eq!(
            parse_vasm_line("jmp $-2"),
            Ok(VASMLine::Instruction(
                None,
                Jmp,
                Some(Node::AbsoluteOffset(-2))
            ))
        );
        assert_eq!(
            parse_vasm_line("brz @+3"),
            Ok(VASMLine::Instruction(
                None,
                Brz,
                Some(Node::RelativeOffset(3))
            ))
        )
    }

    #[test]
    fn test_parse_labels() {
        assert_eq!(
            parse_vasm_line("foo: add"),
            Ok(VASMLine::Instruction(Some(Label("foo")), Add, None))
        );
        assert_eq!(
            parse_vasm_line("bar:"),
            Ok(VASMLine::LabelDef(Label("bar")))
        );
        assert_eq!(
            parse_vasm_line("foo: add 43"),
            Ok(VASMLine::Instruction(
                Some(Label("foo")),
                Add,
                Some(number(43))
            ))
        );
    }

    #[test]
    fn test_parse_directives() {
        assert_eq!(
            parse_vasm_line("foo: .equ 47"),
            Ok(VASMLine::Equ(Label("foo"), number(47)))
        );
        assert_eq!(
            parse_vasm_line(".org 0x400"),
            Ok(VASMLine::Org(None, number(1024)))
        );
    }
}
