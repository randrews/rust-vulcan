#[derive(PartialEq, Clone, Debug)]
pub struct Program(pub Vec<Declaration>);

#[derive(PartialEq, Clone, Debug)]
pub enum Declaration {
    Function(Function),
    Global(Global),
    Struct(Struct),
    Const(Const),
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub struct Varinfo {
    pub typename: Option<String>,
    pub size: Option<i32>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Global {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<i32>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub members: Vec<Member>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Member {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<i32>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Const {
    pub name: String,
    pub value: Option<i32>,
    pub string: Option<String>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block(pub Vec<Statement>);

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub org: Option<i32>,
    pub typename: Option<String>,
    pub inline: bool,
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
    Call(Call),
    VarDecl(VarDecl),
    Conditional(Conditional),
    WhileLoop(WhileLoop),
    RepeatLoop(RepeatLoop),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Return(pub Option<Node>);

#[derive(PartialEq, Clone, Debug)]
pub struct Assignment {
    pub lvalue: Lvalue,
    pub rvalue: Rvalue,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Call {
    pub name: String,
    pub args: Vec<Rvalue>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Lvalue {
    ArrayRef(ArrayRef),
    Name(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Rvalue {
    Expr(Node),
    String(String),
}

#[derive(PartialEq, Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub typename: Option<String>,
    pub size: Option<i32>,
    pub initial: Option<Node>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Conditional {
    pub condition: Node,
    pub body: Block,
    pub alternative: Option<Block>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct WhileLoop {
    pub condition: Node,
    pub body: Block,
}

#[derive(PartialEq, Clone, Debug)]
pub struct RepeatLoop {
    pub count: Node,
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
}

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Number(i32),
    Call(Call),
    ArrayRef(ArrayRef),
    Name(String),
    Address(String),
    Expr(BoxNode, Operator, BoxNode),
}

impl From<i32> for Node {
    fn from(val: i32) -> Self {
        Self::Number(val)
    }
}

impl From<&str> for Node {
    fn from(s: &str) -> Self {
        Self::Name(String::from(s))
    }
}

#[repr(transparent)]
#[derive(PartialEq, Clone, Debug)]
pub struct BoxNode(pub Box<Node>);

impl From<i32> for BoxNode {
    fn from(val: i32) -> Self {
        BoxNode(Box::from(Node::Number(val)))
    }
}

impl From<Node> for BoxNode {
    fn from(val: Node) -> Self {
        BoxNode(Box::from(val))
    }
}

impl From<BoxNode> for Node {
    fn from(val: BoxNode) -> Self {
        *(val.0)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayRef {
    pub name: String,
    pub subscript: BoxNode,
}
