use crate::ast::{Call, Expr, Operator};
use crate::parser::{AstNode, Pair, PairExt, PestRule, PRATT_PARSER};

impl AstNode for Expr {
    const RULE: PestRule = PestRule::expr;
    fn from_pair(pair: Pair) -> Self {
        // The way this works is, the rule has to be of the form:
        // expr = { prefix* ~ val ~ suffix* ~ (operator ~ prefix* ~ val ~ suffix*)* }
        // Each of these map methods turns a thing into an expr. Which means expr HAS
        // to be an enum with the different possible forms these things can take:
        // - if it's a term, it goes into map_primary and returns an Expr::Number or Name
        // - If it's a prefix or suffix, it goes into map_prefix or map_postfix, and
        //   returns an Expr::Prefix or Expr::Suffix
        // - Operators go into map_infix along with two exprs for the left and right
        //   sides
        // The output of all this is an Expr, containing a tree of other Exprs of
        // various forms.
        PRATT_PARSER
            .map_primary(|term| match term.as_rule() {
                PestRule::number => Expr::Number(term.into_number()),
                PestRule::alloc => Expr::New(Expr::from_pair(term.first()).into()),
                PestRule::static_alloc => Expr::Static(Expr::from_pair(term.first()).into()),
                PestRule::name => Expr::Name(String::from(term.as_str())),
                PestRule::expr => Expr::from_pair(term),
                PestRule::string => Self::String(term.into_quoted_string()),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs| Expr::Infix(lhs.into(), Operator::from_pair(op), rhs.into()))
            .map_prefix(|prefix, expr| match prefix.as_str() {
                "-" => Expr::Neg(expr.into()),
                "!" => Expr::Not(expr.into()),
                "*" => Expr::Deref(expr.into()),
                "&" => Expr::Address(expr.into()),
                _ => unreachable!(),
            })
            .map_postfix(|expr, suffix| match suffix.as_rule() {
                PestRule::arglist => Expr::Call(
                    Call {
                        target: expr.into(),
                        args: suffix.into_inner().map(Expr::from_pair).collect(),
                    }
                ),
                PestRule::subscript => {
                    Expr::Subscript(expr.into(), Expr::from_pair(suffix.first()).into())
                }
                _ => unreachable!(),
            })
            .parse(pair.into_inner())
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parseable;
    use super::*;
    use Expr::Infix;
    use Operator::*;
    use crate::ast::Statement;

    #[test]
    fn parse_basic_exprs() {
        // A very, very basic expression
        assert_eq!(Expr::from_str("23"), Ok(Expr::Number(23)));
    }

    #[test]
    fn infix_operator() {
        // Two vals with an operator
        assert_eq!(
            Expr::from_str("23 + 5"),
            Ok(Infix(23.into(), Add, 5.into()))
        );
    }

    #[test]
    fn multiple_operators() {
        // Multiple terms at the same precedence level
        assert_eq!(
            Expr::from_str("1 + 2 + 3"),
            Ok(Infix(Infix(1.into(), Add, 2.into()).into(), Add, 3.into()))
        );
    }

    #[test]
    fn simple_prefix() {
        // Simple prefix
        assert_eq!(Expr::from_str("-5"), Ok(Expr::Neg(5.into())));
    }

    #[test]
    fn multiple_prefixes() {
        // Multiple prefixes
        assert_eq!(
            Expr::from_str("!-foo"),
            Ok(Expr::Not(Expr::Neg("foo".into()).into()))
        );
    }

    #[test]
    fn simple_suffix() {
        // Simple suffix
        assert_eq!(
            Expr::from_str("foo[10]"),
            Ok(Expr::Subscript("foo".into(), 10.into()))
        );
    }

    #[test]
    fn multiple_suffixes() {
        // Multi-suffix
        assert_eq!(
            Expr::from_str("foo[10][3]"),
            Ok(Expr::Subscript(
                Expr::Subscript("foo".into(), 10.into()).into(),
                3.into()
            ))
        );
    }

    #[test]
    fn precedence_levels() {
        // Higher precedence levels
        assert_eq!(
            Expr::from_str("1 + 2 * 3"),
            Ok(Infix(1.into(), Add, Infix(2.into(), Mul, 3.into()).into()))
        );

        assert_eq!(Expr::from_str("2 * 3"), Ok(Infix(2.into(), Mul, 3.into())));

        assert_eq!(
            Expr::from_str("2 * 3 + 4"),
            Ok(Infix(Infix(2.into(), Mul, 3.into()).into(), Add, 4.into()))
        );
    }

    #[test]
    fn weird_ops() {
        // Various operators
        assert_eq!(
            Expr::from_str("1 || 2 && 3"),
            Ok(Infix(1.into(), Or, Infix(2.into(), And, 3.into()).into()))
        );

        assert_eq!(
            Expr::from_str("2 && &blah"),
            Ok(Infix(2.into(), And, Expr::Address("blah".into()).into()))
        );

        assert_eq!(
            Expr::from_str("2 & &blah"),
            Ok(Infix(2.into(), BitAnd, Expr::Address("blah".into()).into()))
        );

        assert_eq!(
            Expr::from_str("1 | 2 ^ 3"),
            Ok(Infix(
                1.into(),
                BitOr,
                Infix(2.into(), Xor, 3.into()).into()
            ))
        );

        assert_eq!(
            Expr::from_str("x == y > z"),
            Ok(Infix(
                "x".into(),
                Eq,
                Infix("y".into(), Gt, "z".into()).into()
            ))
        );

        assert_eq!(
            Expr::from_str("1 << 6"),
            Ok(Infix(1.into(), Lshift, 6.into()))
        );

        assert_eq!(
            Expr::from_str("!a - -3"),
            Ok(Infix(
                Expr::Not("a".into()).into(),
                Sub,
                Expr::Neg(3.into()).into()
            ))
        );

        assert_eq!(
            Expr::from_str("-(4 * 5)"),
            Ok(Expr::Neg(Infix(4.into(), Mul, 5.into()).into()))
        );
    }

    #[test]
    fn parens() {
        // Parens
        assert_eq!(
            Expr::from_str("(1 + 2) * 3"),
            Ok(Infix(Infix(1.into(), Add, 2.into()).into(), Mul, 3.into()))
        );
    }

    #[test]
    fn addresses() {
        // Addresses
        assert_eq!(
            Expr::from_str("&foo[7]"),
            Ok(Expr::Address(Expr::Subscript("foo".into(), 7.into()).into()))
        );

        assert_eq!(
            Expr::from_str("&foo + 7"),
            Ok(Expr::Infix(
                Expr::Address("foo".into()).into(),
                Add,
                7.into()
            ))
        );

        // This is an example of a thing that will parse but not compile. This parses as an address
        // of a call, which doesn't make sense, but because Expr::Address contains an Lvalue the
        // compiler can detect this and error at that stage.
        assert_eq!(
            Expr::from_str("&foo()"),
            Ok(Expr::Address(Expr::Call(Call { target: "foo".into(), args: vec![] }).into()))
        );
    }

    #[test]
    fn dereferencing() {
        // Dereferencing
        assert_eq!(
            Expr::from_str("*foo"),
            Ok(Expr::Deref("foo".into()))
        );

        assert_eq!(
            Expr::from_str("*foo[3]"), // The subscript happens before the dereference
            Ok(Expr::Deref(Expr::Subscript("foo".into(), 3.into()).into()))
        );
    }

    #[test]
    fn expr_call() {
        let blah = Expr::Call(Call { target: "blah".into(), args: vec![] });

        // Can Expr parse a call?
        assert_eq!(Expr::from_str("blah()"), Ok(blah.clone()));
    }

    #[test]
    fn statement_call() {
        let blah = Expr::Call(Call { target: "blah".into(), args: vec![] });

        // Can Statement parse a call?
        assert_eq!(Statement::from_str("blah();"), Ok(Statement::Expr(blah)));
    }

    #[test]
    fn calls_with_args() {
        // Calls with args
        assert_eq!(
            Expr::from_str("blah(1, 2)"),
            Ok(Expr::Call(Call { target: "blah".into(), args: vec![1.into(), 2.into()] }))
        );
    }

    #[test]
    fn calls_with_strings() {
        //Calls with strings
        assert_eq!(
            Expr::from_str("blah(\"foo\", 2)"),
            Ok(Expr::Call(Call { target: "blah".into(), args: vec![Expr::String("foo".into()), 2.into()] }))
        );
    }

    #[test]
    fn alloc() {
        assert_eq!(Expr::from_str("new(8)"), Ok(Expr::New(8.into())));
    }

    #[test]
    fn static_alloc() {
        assert_eq!(Expr::from_str("static(8)"), Ok(Expr::Static(8.into())));
    }
}