use vcore::opcodes::Opcode;

use std::fmt::{Display, Formatter};

/// One of the five arithmetical operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
    String(String), // TODO: this shouldn't exist
    Expr(Box<Node<'a>>, Vec<(Operator, Node<'a>)>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Label<'a>(pub &'a str);

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VASMLine<'a> {
    Instruction(Option<Label<'a>>, Opcode, Option<Node<'a>>),
    Db(Option<Label<'a>>, Node<'a>),
    Org(Option<Label<'a>>, Node<'a>),
    Equ(Label<'a>, Node<'a>),
    LabelDef(Label<'a>),
}
