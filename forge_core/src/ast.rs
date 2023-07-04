#[derive(Eq, PartialEq, Clone, Debug)]
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub org: Option<i32>,
    pub typename: Option<String>,
    pub inline: bool,
    pub args: Vec<Argname>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Argname {
    pub name: String,
    pub typename: Option<String>,
}
