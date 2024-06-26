#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub col: usize
}

impl From<(usize, usize)> for Location {
    fn from(value: (usize, usize)) -> Self {
        Self { line: value.0, col: value.1 }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Located<T> {
    pub location: Location,
    pub ast: T
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program(pub Vec<Located<Declaration>>);

#[derive(PartialEq, Clone, Debug)]
pub enum Declaration {
    Function(Function),
    Global(Global),
    Const(Const),
    Prototype(FunctionPrototype),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Global {
    pub name: String,
    pub initial: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Const {
    pub name: String,
    pub value: Option<Expr>,
    pub string: Option<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block(pub Vec<Located<Statement>>);

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionPrototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Break,
    Continue,
    Return(Return),
    Assignment(Assignment),
    Expr(Expr),
    VarDecl(VarDecl),
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    RepeatLoop(RepeatLoop),
    Asm(Asm),
    Once(Once)
}

#[derive(PartialEq, Clone, Debug)]
pub struct Return(pub Option<Expr>);

#[derive(PartialEq, Clone, Debug)]
pub struct Assignment {
    pub lvalue: Lvalue,
    pub rvalue: Expr,
}

// An lvalue is just a tag on an expr. Any expr parses just fine as an lvalue... not all exprs
// will compile as lvalues. But this saves us essentially having to define the expr grammar twice:
// in the compiler we can tell whether it makes sense for a given lvalue to compile into code that
// yields an address or if it can only yield a value.
#[derive(PartialEq, Clone, Debug)]
pub struct Lvalue(pub BoxExpr);

impl From<&str> for Lvalue {
    fn from(name: &str) -> Self {
        Self(name.into())
    }
}

impl From<String> for Lvalue {
    fn from(name: String) -> Self {
        Self(name.into())
    }
}

impl From<Expr> for Lvalue {
    fn from(value: Expr) -> Self {
        Self(value.into())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub initial: Option<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub body: Block,
    pub alternative: Option<Block>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Once {
    pub body: Block
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

#[derive(PartialEq, Clone, Debug)]
pub struct Asm {
    pub args: Vec<BoxExpr>,
    pub body: String
}

#[derive(PartialEq, Clone, Debug)]
pub struct Call {
    pub target: BoxExpr,
    pub args: Vec<Expr>
}

/// One of the five arithmetical operators
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i32),
    Name(String),
    Not(BoxExpr),
    Neg(BoxExpr),
    Deref(BoxExpr),
    Address(Lvalue),
    Call(Call),
    New(BoxExpr),
    Static(BoxExpr),
    Peek(BoxExpr),
    Poke(BoxExpr, BoxExpr),
    Subscript(BoxExpr, BoxExpr),
    Infix(BoxExpr, Operator, BoxExpr),
    String(String),
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

impl From<String> for BoxExpr {
    fn from(value: String) -> Self {
        Expr::Name(value).into()
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

impl Statement {
    pub fn description(&self) -> String {
        String::from(
            match self {
                Statement::Break => "break",
                Statement::Continue => "continue",
                Statement::Return(_) => "return",
                Statement::Assignment(_) => "assignment",
                Statement::Expr(_) => "expr",
                Statement::VarDecl(_) => "varDecl",
                Statement::Conditional(_) => "conditional",
                Statement::WhileLoop(_) => "whileLoop",
                Statement::RepeatLoop(_) => "repeatLoop",
                Statement::Asm(_) => "asm",
                Statement::Once(_) => "once"
            }
        )
    }
}