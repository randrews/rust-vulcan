use std::convert::TryFrom;
use std::str::FromStr;

use pest::iterators::Pair;

use vcore::opcodes::Opcode;

use crate::parse_error::ParseError;
use crate::vasm_parser::{Rule, VASMParser};

/// A non-opcode directive to the assembler
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Directive {
    Db,
    Equ,
    Org,
}

impl From<Pair<'_, Rule>> for Directive {
    fn from(value: Pair<Rule>) -> Self {
        use Directive::*;
        match value.as_str() {
            ".db" => Db,
            ".equ" => Equ,
            ".org" => Org,
            _ => unreachable!(),
        }
    }
}

/// Either an opcode or a directive; something that lives in the
/// instruction column of a line
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Instruction {
    Directive(Directive),
    Opcode(Opcode),
}

impl<'a> TryFrom<Pair<'a, Rule>> for Instruction {
    type Error = ParseError<'a>;

    fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::opcode => Ok(Instruction::Opcode(Opcode::try_from(pair.as_str())?)),
            Rule::directive => Ok(Instruction::Directive(Directive::from(pair))),
            _ => Err(ParseError::InvalidInstruction(pair.as_str())),
        }
    }
}

/// One of the five arithmetical operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<Pair<'_, Rule>> for Operator {
    fn from(s: Pair<Rule>) -> Self {
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

/// An AST node for an instruction argument. This can be a string, number, label reference,
/// line offset, or an expr containing a sequence of other Nodes joined by same-precedence
/// `Operator`s.
#[derive(Debug, PartialEq, Clone)]
pub enum Node<'a> {
    Number(i32),
    Label(&'a str),
    RelativeLabel(&'a str),
    AbsoluteOffset(i32),
    RelativeOffset(i32),
    String(String),
    Expr(Box<Node<'a>>, Vec<(Operator, Node<'a>)>),
}

impl<'a> Node<'a> {
    /// Create a `Node` containing the string represented by a given pair. Since the pair will
    /// reference bytes containing escape sequences, this isn't the same as an &str to the
    /// original code; this is a new string translating those escape sequences to their actual
    /// bytes.
    fn string(pair: Pair<'a, Rule>) -> Node<'a> {
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
                _ => string.push_str(string_inner),
            }
        }
        Node::String(string)
    }

    /// Create a `Node` containing the line offset represented by a pair.
    fn line_offset(pair: Pair<'a, Rule>) -> Node<'a> {
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
}

impl<'a> TryFrom<Pair<'a, Rule>> for Node<'a> {
    type Error = ParseError<'a>;

    /// Attempt to parse a given pair into the `Node` it represents. The result of this is a
    /// `Node` that might contain a borrow of part of the original code, but which in no way
    /// depends on the original pair's object model.
    fn try_from(pair: Pair<'a, Rule>) -> Result<Self, Self::Error> {
        match pair.as_rule() {
            Rule::expr | Rule::term => {
                let mut iter = pair.into_inner();
                let first = Node::try_from(iter.next().unwrap())?;
                let mut rest = Vec::<(Operator, Node)>::new();

                while let Some(operator) = iter.next() {
                    let rhs = iter.next().unwrap();
                    let op = Operator::from(operator);
                    let node = Node::try_from(rhs)?;
                    rest.push((op, node));
                }
                Ok(Node::Expr(Box::from(first), rest))
            }
            Rule::fact | Rule::number => Ok(Node::try_from(pair.into_inner().next().unwrap())?),
            Rule::dec_number | Rule::dec_zero | Rule::neg_number => {
                Ok(Node::Number(i32::from_str(pair.as_str()).unwrap()))
            }
            Rule::hex_number => Ok(Node::Number(
                i32::from_str_radix(pair.as_str().get(2..).unwrap(), 16).unwrap(),
            )),
            Rule::bin_number => Ok(Node::Number(
                i32::from_str_radix(pair.as_str().get(2..).unwrap(), 2).unwrap(),
            )),
            Rule::oct_number => Ok(Node::Number(
                i32::from_str_radix(pair.as_str().get(2..).unwrap(), 8).unwrap(),
            )),
            Rule::string => Ok(Node::string(pair)),
            Rule::label => Ok(Node::Label(pair.as_str())),
            Rule::relative_label => Ok(Node::RelativeLabel(pair.as_str().get(1..).unwrap())),
            Rule::absolute_line_offset | Rule::relative_line_offset => Ok(Node::line_offset(pair)),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VASMLine<'a> {
    label: Option<&'a str>,
    instruction: Option<Instruction>,
    argument: Option<Node<'a>>,
}

impl<'a> VASMLine<'a> {
    pub fn blank() -> VASMLine<'a> {
        VASMLine {
            label: None,
            instruction: None,
            argument: None,
        }
    }

    pub fn with_instruction(self, instruction: Instruction) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(instruction),
            argument: self.argument,
        }
    }

    pub fn with_opcode(self, opcode: Opcode) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(Instruction::Opcode(opcode)),
            argument: self.argument,
        }
    }

    pub fn with_directive(self, directive: Directive) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            instruction: Some(Instruction::Directive(directive)),
            argument: self.argument,
        }
    }

    pub fn with_label(self, label: &'a str) -> Self {
        VASMLine {
            label: Some(label),
            instruction: self.instruction,
            argument: self.argument,
        }
    }

    pub fn with_argument(self, argument: Node<'a>) -> Self {
        VASMLine {
            label: self.label,
            instruction: self.instruction,
            argument: Some(argument),
        }
    }
}
