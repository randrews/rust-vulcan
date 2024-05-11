use crate::ast::*;
use crate::parser::{AstNode, Pair, PairExt, PestRule};

impl AstNode for Statement {
    const RULE: PestRule = PestRule::statement;
    fn from_pair(pair: Pair) -> Self {
        let pair = pair.first();
        match pair.as_rule() {
            PestRule::break_stmt => Self::Break,
            PestRule::continue_stmt => Self::Continue,
            PestRule::return_stmt => Self::Return(Return::from_pair(pair)),
            PestRule::assignment => Self::Assignment(Assignment::from_pair(pair)),
            PestRule::expr => Self::Expr(Expr::from_pair(pair)),
            PestRule::var_decl => Self::VarDecl(VarDecl::from_pair(pair)),
            PestRule::conditional => Self::Conditional(Conditional::from_pair(pair)),
            PestRule::while_loop => Self::WhileLoop(WhileLoop::from_pair(pair)),
            PestRule::repeat_loop => Self::RepeatLoop(RepeatLoop::from_pair(pair)),
            PestRule::once => Self::Once(Once::from_pair(pair)),
            PestRule::asm => Self::Asm(Asm::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}