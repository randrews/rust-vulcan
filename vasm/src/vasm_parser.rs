use vcore::opcodes::Opcode;

use crate::parse_error::ParseError;

use pest::{Parser};
use std::convert::TryFrom;
use pest::iterators::Pair;
use std::str::FromStr;

#[derive(Parser)]
#[grammar = "vasm.pest"]
struct VASMParser;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Directive { Db, Equ, Org }

impl From<&str> for Directive {
    fn from(value: &str) -> Self {
        use Directive::*;
        match value {
            ".db" => Db,
            ".equ" => Equ,
            ".org" => Org,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Instruction {
    Directive(Directive),
    Opcode(Opcode)
}

impl<'a> TryFrom<Pair<'a, Rule>> for Instruction {
    type Error = ParseError<'a>;

    fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::opcode => Ok(Instruction::Opcode(Opcode::try_from(pair.as_str())?)),
            Rule::directive => Ok(Instruction::Directive(Directive::from(pair.as_str()))),
            _ => Err(ParseError::InvalidInstruction(pair.as_str()))
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Operator { Add, Sub, Mul, Div, Mod }

impl From<&str> for Operator {
    fn from(s: &str) -> Self {
        use Operator::*;
        match s {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "%" => Mod,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Node<'a>{
    Number(i32),
    Label(&'a str),
    RelativeLabel(&'a str),
    AbsoluteOffset(i32),
    RelativeOffset(i32),
    String(String),
    Expr(Box<Node<'a>>, Vec<(Operator, Node<'a>)>)
}

impl<'a> Node<'a> {
    pub fn simple_expr(node: Node<'a>) -> Node<'a> {
        Node::Expr(Box::from(node), vec![])
    }

    pub fn string(pair: Pair<'a, Rule>) -> Node<'a> {
        let mut string = String::with_capacity(pair.as_str().len());
        for inner in pair.into_inner() {
            let string_inner = inner.as_str();
            match string_inner {
                "\\t" => string.push_str("\t"),
                "\\r" => string.push_str("\r"),
                "\\n" => string.push_str("\n"),
                "\\0" => string.push_str("\0"),
                "\\\\" => string.push_str("\\"),
                "\\\"" => string.push_str("\""),
                _ => string.push_str(string_inner)
            }
        }
        Node::String(string)
    }

    pub fn line_offset(pair: Pair<'a, Rule>) -> Node<'a> {
        let outer = pair.as_rule();
        let mut inner = pair.into_inner();
        let sign = inner.next().unwrap().as_str();
        let mut num = i32::from_str(inner.next().unwrap().as_str()).unwrap();
        if sign == "-" { num *= -1 }
        match outer {
            Rule::relative_line_offset => Node::RelativeOffset(num),
            Rule::absolute_line_offset => Node::AbsoluteOffset(num),
            _ => unreachable!()
        }
    }

    pub fn shake(node: Node<'a>) -> Node<'a> {
        match node {
            Node::Expr(first, rest) => {
                let shaken_car = Node::shake(*first);
                if rest.is_empty() {
                    shaken_car
                } else {
                    let shaken_cdr = rest.into_iter().map(|(op, n)| (op, Node::shake(n))).collect();
                    Node::Expr(Box::from(shaken_car), shaken_cdr)
                }
            },
            _ => node
        }
    }
}

impl<'a> TryFrom<Pair<'a, Rule>> for Node<'a> {
    type Error = ParseError<'a>;

    fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::expr | Rule::term => {
                let mut iter = pair.into_inner();
                let first = Node::try_from(iter.next().unwrap())?;
                let mut rest = Vec::<(Operator, Node)>::new();

                while let Some(operator) = iter.next() {
                    let rhs = iter.next().unwrap();
                    let op = Operator::from(operator.as_str());
                    let node = Node::try_from(rhs)?;
                    rest.push((op, node));
                }
                Ok(Node::Expr(Box::from(first), rest))
            }
            Rule::fact | Rule::number => {
                Ok(Node::try_from(pair.into_inner().next().unwrap())?)
            }
            Rule::dec_number | Rule::dec_zero | Rule::neg_number => {
                Ok(Node::Number(i32::from_str(pair.as_str()).unwrap()))
            }
            Rule::hex_number => { Ok(Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 16).unwrap())) }
            Rule::bin_number => { Ok(Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 2).unwrap())) }
            Rule::oct_number => { Ok(Node::Number(i32::from_str_radix(pair.as_str().get(2..).unwrap(), 8).unwrap())) }
            Rule::string => { Ok(Node::string(pair)) }
            Rule::label => Ok(Node::Label(pair.as_str())),
            Rule::relative_label => Ok(Node::RelativeLabel(pair.as_str().get(1..).unwrap())),
            Rule::absolute_line_offset | Rule::relative_line_offset => Ok(Node::line_offset(pair)),
            _ => todo!()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VASMLine<'a> {
    label: Option<&'a str>,
    instruction: Option<Instruction>,
    argument: Option<Node<'a>>
}

impl<'a> VASMLine<'a> {
    pub fn blank() -> VASMLine<'a> {
        VASMLine {
            label: None,
            instruction: None,
            argument: None
        }
    }

    pub fn with_instruction(self, instruction: Instruction) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(instruction),
            argument: self.argument
        }
    }

    pub fn with_opcode(self, opcode: Opcode) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(Instruction::Opcode(opcode)),
            argument: self.argument
        }
    }

    pub fn with_directive(self, directive: Directive) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(Instruction::Directive(directive)),
            argument: self.argument
        }
    }

    pub fn with_label(self, label: &'a str) -> Self {
        VASMLine {
            label: Some(label),
            instruction: self.instruction,
            argument: self.argument
        }
    }

    pub fn with_argument(self, argument: Node<'a>) -> Self {
        VASMLine {
            label: self.label,
            instruction: self.instruction,
            argument: Some(argument)
        }
    }
}

fn parse_vasm_line(line: &str) -> Result<VASMLine, ParseError<'_>> {
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

    if let Some(instruction_group) = pairs.next_if(|pair| pair.as_rule() == Rule::instruction_group) {
        let mut pairs = instruction_group.into_inner();
        vasm_line = vasm_line.with_instruction(Instruction::try_from(pairs.next().unwrap())?);

        if let Some(argument_group) = pairs.next() {
            vasm_line = vasm_line.with_argument(Node::shake(Node::try_from(argument_group)?))
        }
    }

    Ok(vasm_line)
}

#[cfg(test)]
mod test {
    use super::*;
    use Opcode::*;
    use Directive::*;

    impl<'a> VASMLine<'a> {
        pub fn op(opcode: Opcode) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode)
        }

        fn op_number(opcode: Opcode, argument: i32) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode).with_argument(Node::Number(argument))
        }

        fn op_label(opcode: Opcode, argument: &'a str) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode).with_argument(Node::Label(argument))
        }

        fn op_rel_label(opcode: Opcode, argument: &'a str) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode).with_argument(Node::RelativeLabel(argument))
        }

        fn op_off(opcode: Opcode, argument: Node<'a>) -> VASMLine<'a> {
            Self::blank().with_opcode(opcode).with_argument(argument)
        }

        pub fn dir_str(directive: Directive, string: &str) -> VASMLine<'a> {
            Self::blank().with_directive(directive).with_argument(Node::String(string.to_string()))
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse_vasm_line("add"), Ok(VASMLine::op(Add)));
        assert_eq!(parse_vasm_line("sub"), Ok(VASMLine::op(Sub)));
        assert_eq!(parse_vasm_line("blah"), Err(ParseError::InvalidInstruction("blah")));
        assert_eq!(parse_vasm_line("47"), Err(ParseError::LineParseFailure));
    }

    #[test]
    fn test_numbers() {
        assert_eq!(parse_vasm_line("add 45"), Ok(VASMLine::op_number(Add, 45)));
        assert_eq!(parse_vasm_line("add 0"), Ok(VASMLine::op_number(Add, 0)));
        assert_eq!(parse_vasm_line("add 0x10"), Ok(VASMLine::op_number(Add, 16)));
        assert_eq!(parse_vasm_line("add 0b1111"), Ok(VASMLine::op_number(Add, 15)));
        assert_eq!(parse_vasm_line("add 0o377"), Ok(VASMLine::op_number(Add, 255)));
        assert_eq!(parse_vasm_line("add -17"), Ok(VASMLine::op_number(Add, -17)));
    }

    #[test]
    fn test_strings() {
        assert_eq!(parse_vasm_line(".db \"blah\""), Ok(VASMLine::dir_str(Db, "blah")));
        assert_eq!(parse_vasm_line(".db \"blah\\twith escapes\\0\""), Ok(VASMLine::dir_str(Db, "blah\twith escapes\0")))
    }

    #[test]
    fn test_exprs() {
        assert_eq!(parse_vasm_line("add 2 + 3 * (4 - 5) + 6"), Ok(
            VASMLine::op(Add)
                .with_argument(
                    Node::Expr(
                        Box::from(Node::Number(2)),
                        vec![
                            (
                                Operator::Add,
                                Node::Expr(
                                    Box::from(Node::Number(3)),
                                    vec![
                                        (
                                            Operator::Mul,
                                            Node::Expr(
                                                Box::from(Node::Number(4)),
                                                vec![(Operator::Sub, Node::Number(5))],
                                            ))],)),
                            (
                                Operator::Add,
                                Node::Number(6)
                            )],))));
    }

    #[test]
    fn test_expr_labels() {
        assert_eq!(parse_vasm_line("loadw foo"), Ok(VASMLine::op_label(Loadw, "foo")));
        assert_eq!(parse_vasm_line("brz @blah"), Ok(VASMLine::op_rel_label(Brz, "blah")))
    }

    #[test]
    fn test_expr_offsets() {
        assert_eq!(parse_vasm_line("jmp $-2"), Ok(VASMLine::op_off(Jmp, Node::AbsoluteOffset(-2))));
        assert_eq!(parse_vasm_line("brz @+3"), Ok(VASMLine::op_off(Brz, Node::RelativeOffset(3))))
    }

    #[test]
    fn test_parse_labels() {
        assert_eq!(parse_vasm_line("foo: add"), Ok(VASMLine::op(Add).with_label("foo")));
        assert_eq!(parse_vasm_line("bar:"), Ok(VASMLine::blank().with_label("bar")));
        assert_eq!(parse_vasm_line("foo: add 43"), Ok(VASMLine::op_number(Add, 43).with_label("foo")));
    }

    #[test]
    fn test_parse_directives() {
        assert_eq!(parse_vasm_line("foo: .equ"), Ok(VASMLine::blank().with_label("foo").with_directive(Equ)));
        assert_eq!(parse_vasm_line(".db"), Ok(VASMLine::blank().with_directive(Db)));
    }
}
