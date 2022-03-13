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

#[derive(Debug, PartialEq, Clone)]
enum Node<'a>{
    Number(i32),
    Label(&'a str),
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
}

impl<'a> TryFrom<Pair<'a, Rule>> for Node<'a> {
    type Error = ParseError<'a>;

    fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::expr | Rule::term => {
                let first = Node::try_from(pair.into_inner().next().unwrap())?;
                Ok(Node::Expr(Box::from(first), vec![]))
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
            vasm_line = match argument_group.as_rule() {
                Rule::string => vasm_line.with_argument(Node::try_from(argument_group)?),
                Rule::expr => vasm_line.with_argument(Node::try_from(argument_group.into_inner().next().unwrap())?),
                _ => unreachable!()
            }
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
            Self::blank().with_opcode(opcode).with_argument(Node::simple_expr(Node::Number(argument)))
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
