use crate::ast::{Expr, Return};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Return {
    const RULE: PestRule = PestRule::return_stmt;
    fn from_pair(pair: Pair) -> Self {
        pair.into_inner()
            .next()
            .map_or(Self(None), |expr| Self(Some(Expr::from_pair(expr))))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_return() {
        assert_eq!(
            Statement::from_str("return;"),
            Ok(Statement::Return(Return(None)))
        );
    }

    #[test]
    fn parse_return_with_value() {
        assert_eq!(
            Statement::from_str("return 17;"),
            Ok(Statement::Return(Return(Some(17.into()))))
        );
    }
}