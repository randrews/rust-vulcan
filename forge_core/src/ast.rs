#[derive(PartialEq, Clone, Debug)]
pub struct Program(pub Vec<Declaration>);

#[derive(PartialEq, Clone, Debug)]
pub enum Declaration {
    Function(Function),
    Global(Global),
    Struct(Struct),
    Const(Const),
}

#[derive(PartialEq, Clone, Debug, Default)]
pub struct Varinfo {
    pub typename: Option<String>,
    pub size: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Global {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub members: Vec<Member>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Member {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Const {
    pub name: String,
    pub value: Option<Expr>,
    pub string: Option<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block(pub Vec<Statement>);

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Argname>,
    pub body: Block,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Argname {
    pub name: String,
    pub typename: Option<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Return(Return),
    Assignment(Assignment),
    Expr(Expr),
    VarDecl(VarDecl),
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    RepeatLoop(RepeatLoop),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Return(pub Option<Expr>);

#[derive(PartialEq, Clone, Debug)]
pub struct Assignment {
    pub lvalue: Lvalue,
    pub rvalue: Rvalue,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Lvalue {
    ArrayRef(ArrayRef),
    Name(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Rvalue {
    Expr(Expr),
    String(String),
}

#[derive(PartialEq, Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<Expr>,
    pub initial: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub body: Block,
    pub alternative: Option<Block>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Block,
}

#[derive(PartialEq, Clone, Debug)]
pub struct RepeatLoop {
    pub count: Expr,
    pub name: Option<String>,
    pub body: Block,
}

/// One of the five arithmetical operators
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    BitAnd,
    BitOr,
    Xor,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Lshift,
    Rshift,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Prefix {
    Neg,
    Not,
    Address
}

#[derive(Debug, PartialEq, Clone)]
pub enum Suffix {
    Subscript(Expr),
    Arglist(Vec<Rvalue>),
    Member(String),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Expr {
    pub lhs: Val,
    pub op: Option<Operator>,
    pub rhs: Option<BoxExpr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Val {
    Number(i32, Vec<Prefix>, Vec<Suffix>),
    Name(String, Vec<Prefix>, Vec<Suffix>),
    Expr(BoxExpr, Vec<Prefix>, Vec<Suffix>),
}

impl Val {
    pub fn is_simple(&self) -> bool {
        if let Self::Expr(expr, pre, suf) = &self {
            pre.is_empty() && suf.is_empty() && expr.0.op.is_none()
        } else {
            false
        }
    }

    pub fn inner_expr(self) -> Option<Expr> {
        if let Self::Expr(expr, _, _) = self {
            Some(expr.into())
        } else { None }
    }
}

impl From<i32> for Val {
    fn from(val: i32) -> Self {
        Self::Number(val, vec![], vec![])
    }
}

impl From<&str> for Val {
    fn from(s: &str) -> Self {
        Self::Name(String::from(s), vec![], vec![])
    }
}

impl From<Expr> for Val {
    fn from(value: Expr) -> Self {
        Self::Expr(value.into(), vec![], vec![])
    }
}

impl From<BoxExpr> for Val {
    fn from(value: BoxExpr) -> Self {
        Self::Expr(value, vec![], vec![])
    }
}

#[repr(transparent)]
#[derive(PartialEq, Clone, Debug)]
pub struct BoxExpr(pub Box<Expr>);

impl From<Val> for Expr {
    fn from(value: Val) -> Self {
        Self {
            lhs: value,
            op: None,
            rhs: None
        }
    }
}

impl From<i32> for BoxExpr {
    fn from(val: i32) -> Self {
        BoxExpr(Box::from(Expr::from(Val::from(val))))
    }
}

impl From<Expr> for BoxExpr {
    fn from(val: Expr) -> Self {
        BoxExpr(Box::from(val))
    }
}

impl From<BoxExpr> for Expr {
    fn from(val: BoxExpr) -> Self {
        *(val.0)
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Self {
            lhs: Val::Number(value, vec![], vec![]),
            op: None,
            rhs: None
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayRef {
    pub name: String,
    pub subscript: BoxExpr,
}
