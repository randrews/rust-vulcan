use std::convert::TryFrom;

use pest::Parser;

use vcore::opcodes::Opcode;

use crate::ast::{Label, Node, Operator, VASMLine};
use crate::parse_error::ParseError;
use std::str::FromStr;

mod inner {
    #[derive(Parser)]
    #[grammar = "vasm.pest"]
    pub struct VASMParser;
}

use inner::*;

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

impl From<Pair<'_>> for Operator {
    fn from(s: Pair) -> Self {
        use Operator::*;
        match s.as_str() {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "%" => Mod,
            _ => unreachable!(),
        }
    }
}

/// Create a String containing the string represented by a given pair. Since the pair will
/// reference bytes containing escape sequences, this isn't the same as an &str to the
/// original code; this is a new string translating those escape sequences to their actual
/// bytes.
fn create_string(pair: Pair) -> String {
    let mut string = String::with_capacity(pair.as_str().len());
    for inner in pair.into_inner() {
        let string_inner = inner.as_str();
        match string_inner {
            "\\t" => string.push('\t'),
            "\\r" => string.push('\r'),
            "\\n" => string.push('\n'),
            "\\0" => string.push('\0'),
            "\\\\" => string.push('\\'),
            "\\\"" => string.push('\"'),
            _ => string.push_str(string_inner),
        }
    }
    string
}

/// Create a `Node` containing the line offset represented by a pair.
fn create_line_offset_node(pair: Pair) -> Node {
    let outer = pair.as_rule();
    let mut inner = pair.into_inner();
    let sign = inner.next().unwrap().as_str();
    let mut num = i32::from_str(inner.next().unwrap().as_str()).unwrap();
    if sign == "-" {
        num *= -1
    }
    match outer {
        Rule::relative_line_offset => Node::RelativeOffset(num),
        Rule::absolute_line_offset => Node::AbsoluteOffset(num),
        _ => unreachable!(),
    }
}

/// Parse a given pair into the `Node` it represents. The result of this is a
/// `Node` that might contain a borrow of part of the original code, but which in no way
/// depends on the original pair's object model.
fn parse(pair: Pair) -> Node {
    match pair.as_rule() {
        Rule::expr | Rule::term => {
            let mut iter = pair.into_inner();
            let first = parse(iter.next().unwrap());
            let mut rest = Vec::<(Operator, Node)>::new();

            while let Some(operator) = iter.next() {
                let rhs = iter.next().unwrap();
                let op = Operator::from(operator);
                let node = parse(rhs);
                rest.push((op, node));
            }
            Node::Expr(Box::from(first), rest)
        }
        Rule::fact | Rule::number => parse(pair.only()),
        Rule::dec_number | Rule::dec_zero | Rule::neg_number => {
            Node::Number(i32::from_str(pair.as_str()).unwrap())
        }
        Rule::hex_number => {
            Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 16).unwrap())
        }
        Rule::bin_number => {
            Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 2).unwrap())
        }
        Rule::oct_number => {
            Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 8).unwrap())
        }
        Rule::label => Node::Label(pair.as_str()),
        Rule::relative_label => Node::RelativeLabel(pair.as_str().get(1..).unwrap()),
        Rule::absolute_line_offset | Rule::relative_line_offset => create_line_offset_node(pair),
        _ => unreachable!(),
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
                .map(|expr| shake(parse(expr)));
            return Ok(VASMLine::Instruction(label, opcode, argument));
        }
        Rule::db_word => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let argument = shake(parse(iter.next().unwrap()));
            return Ok(VASMLine::Db(label, argument));
        }
        Rule::db_string => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let argument = create_string(iter.next().unwrap());
            return Ok(VASMLine::StringDb(label, argument));
        }
        Rule::org_directive => {
            let mut iter = line.into_inner().peekable();
            let label = optional_label(iter.next_if(|pair| pair.as_rule() == Rule::label_def));
            let argument = shake(parse(iter.next().unwrap()));
            return Ok(VASMLine::Org(label, argument));
        }
        Rule::equ_directive => {
            let mut iter = line.into_inner();
            let label = Label(iter.next().unwrap().only().as_str());
            let argument = shake(parse(iter.next().unwrap()));
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
            Ok(VASMLine::StringDb(None, "blah".into()))
        );
        assert_eq!(
            parse_vasm_line(".db \"blah\\twith escapes\\0\""),
            Ok(VASMLine::StringDb(None, "blah\twith escapes\0".into()))
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
