use vcore::opcodes::Opcode;

use std::collections::BTreeMap;
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
pub enum Node {
    Number(i32),
    Label(String),
    RelativeLabel(String),
    AbsoluteOffset(i32),
    RelativeOffset(i32),
    Expr(Box<Node>, Vec<(Operator, Node)>),
}

impl Node {
    pub fn label(lbl: &str) -> Self {
        Self::Label(lbl.to_string())
    }
    pub fn relative_label(lbl: &str) -> Self {
        Self::RelativeLabel(lbl.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Label(pub String);

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for Label {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

pub type Scope = BTreeMap<String, i32>;

#[derive(Debug, PartialEq, Clone)]
pub enum Macro {
    Include(String),
    If,
    Unless,
    Else,
    While,
    Until,
    Do,
    End,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VASMLine {
    Instruction(Option<Label>, Opcode, Option<Node>),
    Db(Option<Label>, Node),
    StringDb(Option<Label>, String),
    Org(Option<Label>, Node),
    Equ(Label, Node),
    LabelDef(Label),
    Macro(Macro),
    Blank,
}

impl VASMLine {
    pub fn label(&self) -> Option<&Label> {
        match self {
            VASMLine::Instruction(Some(lbl), _, _)
            | VASMLine::Db(Some(lbl), _)
            | VASMLine::StringDb(Some(lbl), _)
            | VASMLine::Org(Some(lbl), _)
            | VASMLine::Equ(lbl, _)
            | VASMLine::LabelDef(lbl) => Some(lbl),
            _ => None,
        }
    }
    pub fn zero_length(&self) -> bool {
        matches!(
            self,
            VASMLine::Org(_, _) | VASMLine::Equ(_, _) | VASMLine::LabelDef(_) | VASMLine::Macro(_)
        )
    }
}
