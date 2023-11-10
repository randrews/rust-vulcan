use crate::ast::{Assignment, Expr};
use crate::parser::{AstNode, Pair, PestRule};

impl AstNode for Assignment {
    const RULE: PestRule = PestRule::assignment;
    fn from_pair(pair: Pair) -> Self {
        let mut pairs = pair.into_inner();
        let lvalue = Expr::from_pair(pairs.next().unwrap()).into();
        let rvalue = Expr::from_pair(pairs.next().unwrap());
        Self { lvalue, rvalue }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::parser::Parseable;
    use super::*;

    #[test]
    fn parse_assignment() {
        assert_eq!(
            Statement::from_str("foo = 7;"),
            Ok(Statement::Assignment(Assignment {
                lvalue: "foo".into(),
                rvalue: 7.into(),
            }))
        );
    }

    #[test]
    fn subscript_assignment() {
        assert_eq!(
            Assignment::from_str("foo[45] = 7"),
            Ok(Assignment {
                lvalue: Expr::Subscript("foo".into(), 45.into()).into(),
                rvalue: 7.into(),
            })
        );
    }

    #[test]
    fn pointer_assignment() {
        assert_eq!(
            Assignment::from_str("*foo = 12"),
            Ok(Assignment {
                lvalue: Expr::Deref("foo".into()).into(),
                rvalue: 12.into()
            })
        );
    }
}