use crate::ast::{Expr, VarDecl};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for VarDecl {
    const RULE: PestRule = PestRule::var_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let initial = inner.next().map(Expr::from_pair);

        Self { name, initial }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_var_decl() {
        assert_eq!(
            Statement::from_str("var blah;"),
            Ok(Statement::VarDecl(VarDecl {
                name: "blah".into(),
                initial: None,
            }))
        );
    }

    #[test]
    fn parse_var_decl_with_value() {
        assert_eq!(
            VarDecl::from_str("var blah = 35"),
            Ok(VarDecl {
                name: "blah".into(),
                initial: Some(35.into()),
            })
        );
    }
}