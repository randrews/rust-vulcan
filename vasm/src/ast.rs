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
    Expr(Box<Node<'a>>, Vec<(Operator, Node<'a>)>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
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
    StringDb(Option<Label<'a>>, String),
    Org(Option<Label<'a>>, Node<'a>),
    Equ(Label<'a>, Node<'a>),
    LabelDef(Label<'a>),
}

impl<'a> VASMLine<'a> {
    pub fn label(&self) -> Option<Label<'a>> {
        match self {
            VASMLine::Instruction(Some(lbl), _, _)
            | VASMLine::Db(Some(lbl), _)
            | VASMLine::StringDb(Some(lbl), _)
            | VASMLine::Org(Some(lbl), _)
            | VASMLine::Equ(lbl, _)
            | VASMLine::LabelDef(lbl) => Some(*lbl),
            _ => None,
        }
    }
    pub fn zero_length(&self) -> bool {
        matches!(
            self,
            VASMLine::Org(_, _) | VASMLine::Equ(_, _) | VASMLine::LabelDef(_)
        )
    }
}
