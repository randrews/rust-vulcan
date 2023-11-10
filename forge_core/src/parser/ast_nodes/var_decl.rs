use crate::ast::{Expr, VarDecl};
use crate::parser::{AstNode, Pair, PairExt, PestRule};

impl AstNode for VarDecl {
    const RULE: PestRule = PestRule::var_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let mut size = None;
        let mut initial = None;

        for p in inner {
            match p.as_rule() {
                PestRule::size => { size = Some(Expr::from_pair(p.first())) }
                PestRule::expr => { initial = Some(Expr::from_pair(p)) }
                _ => unreachable!()
            }
        }

        Self { name, size, initial }
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
                size: None,
                initial: None,
            }))
        );
    }

    #[test]
    fn parse_var_decl_with_value() {
        assert_eq!(
            VarDecl::from_str("var blah[7] = 35"),
            Ok(VarDecl {
                name: "blah".into(),
                size: Some(7.into()),
                initial: Some(35.into()),
            })
        );
    }
}