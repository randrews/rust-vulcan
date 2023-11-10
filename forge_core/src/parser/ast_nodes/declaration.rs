use crate::ast::{Const, Declaration, Function, FunctionPrototype, Global};
use crate::parser::{AstNode, Pair, PestRule, PairExt};

impl AstNode for Declaration {
    const RULE: PestRule = PestRule::declaration;
    fn from_pair(pair: Pair) -> Self {
        let pair = pair.first();
        match pair.as_rule() {
            PestRule::function => Self::Function(Function::from_pair(pair)),
            PestRule::global => Self::Global(Global::from_pair(pair)),
            PestRule::const_decl => Self::Const(Const::from_pair(pair)),
            PestRule::function_prototype => Self::Prototype(FunctionPrototype::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}