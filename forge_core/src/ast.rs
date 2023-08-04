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
pub struct Lvalue {
    pub name: String,
    pub subscripts: Vec<Suffix>,
}

impl From<&str> for Lvalue {
    fn from(name: &str) -> Self {
        Self { name: String::from(name), subscripts: vec![] }
    }
}

impl From<String> for Lvalue {
    fn from(name: String) -> Self {
        Self { name, subscripts: vec![] }
    }
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
    pub initial: Option<Rvalue>,
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
    Subscript(BoxExpr),
    Arglist(Vec<Rvalue>),
    Member(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i32),
    Name(String),
    Expr(BoxExpr),
    Prefix(Prefix, BoxExpr),
    Suffix(BoxExpr, Suffix),
    Infix(BoxExpr, Operator, BoxExpr)
}

#[repr(transparent)]
#[derive(PartialEq, Clone, Debug)]
pub struct BoxExpr(pub Box<Expr>);

impl From<i32> for BoxExpr {
    fn from(val: i32) -> Self {
        BoxExpr(Box::from(Expr::Number(val)))
    }
}

impl From<Expr> for BoxExpr {
    fn from(expr: Expr) -> Self {
        BoxExpr(Box::from(expr))
    }
}

impl From<&str> for BoxExpr {
    fn from(value: &str) -> Self {
        Expr::Name(String::from(value)).into()
    }
}

impl From<BoxExpr> for Expr {
    fn from(expr: BoxExpr) -> Self {
        *(expr.0)
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Self::Number(value)
    }
}

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        Self::Name(String::from(value))
    }
}

impl From<i32> for Rvalue {
    fn from(value: i32) -> Self {
        Self::Expr(value.into())
    }
}

impl From<&str> for Rvalue {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}
