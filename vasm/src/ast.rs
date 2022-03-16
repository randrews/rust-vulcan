use std::str::FromStr;

use pest::iterators::Pair;

use vcore::opcodes::Opcode;

use crate::vasm_parser::Rule;

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
                "\\t" => string.push('\t'),
                "\\r" => string.push('\r'),
                "\\n" => string.push('\n'),
                "\\0" => string.push('\0'),
                "\\\\" => string.push('\\'),
                "\\\"" => string.push('\"'),
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

impl<'a> From<Pair<'a, Rule>> for Node<'a> {
    /// Parse a given pair into the `Node` it represents. The result of this is a
    /// `Node` that might contain a borrow of part of the original code, but which in no way
    /// depends on the original pair's object model.
    fn from(pair: Pair<'a, Rule>) -> Self {
        match pair.as_rule() {
            Rule::expr | Rule::term => {
                let mut iter = pair.into_inner();
                let first = Node::from(iter.next().unwrap());
                let mut rest = Vec::<(Operator, Node)>::new();

                while let Some(operator) = iter.next() {
                    let rhs = iter.next().unwrap();
                    let op = Operator::from(operator);
                    let node = Node::from(rhs);
                    rest.push((op, node));
                }
                Node::Expr(Box::from(first), rest)
            }
            Rule::fact | Rule::number => Node::from(pair.into_inner().next().unwrap()),
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
            Rule::string => Node::string(pair),
            Rule::label => Node::Label(pair.as_str()),
            Rule::relative_label => Node::RelativeLabel(pair.as_str().get(1..).unwrap()),
            Rule::absolute_line_offset | Rule::relative_line_offset => Node::line_offset(pair),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Label<'a>(pub &'a str);

#[derive(Debug, PartialEq, Clone)]
pub enum VASMLine<'a> {
    Instruction(Option<Label<'a>>, Opcode, Option<Node<'a>>),
    Db(Option<Label<'a>>, Node<'a>),
    Org(Option<Label<'a>>, Node<'a>),
    Equ(Label<'a>, Node<'a>),
    LabelDef(Label<'a>),
}
