use pest::pratt_parser::{Op, PrattParser};
use pest::Parser;

#[derive(Parser)]
#[grammar = "new.pest"]
struct ForgeParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(log_or, Left))
            .op(Op::infix(log_and, Left))
            .op(Op::infix(bit_or, Left))
            .op(Op::infix(xor, Left))
            .op(Op::infix(bit_and, Left))
            .op(Op::infix(eq, Left) | Op::infix(ne, Left))
            .op(Op::infix(gt, Left) | Op::infix(ge, Left) | Op::infix(lt, Left) | Op::infix(le, Left))
            .op(Op::infix(lshift, Left) | Op::infix(rshift, Left))
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mul, Left) | Op::infix(div, Left) | Op::infix(modulus, Left))
            .op(Op::prefix(Rule::prefix))
            .op(Op::postfix(Rule::suffix))
    };
}

use crate::ast::*;
use pest::error::{Error, LineColLocation};
use std::iter::Peekable;
use std::str::FromStr;

pub(crate) type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub(crate) type Pairs<'i, R = Rule> = pest::iterators::Pairs<'i, R>;

trait PairExt {
    fn first(self) -> Self;
    fn first_as_string(self) -> String;
    fn only(self) -> Self;
    fn into_number(self) -> i32;

    /// Create a String containing the string represented by a given pair. Since the pair will
    /// reference bytes containing escape sequences, this isn't the same as an &str to the
    /// original code; this is a new string translating those escape sequences to their actual
    /// bytes.
    fn into_quoted_string(self) -> String;
}

impl<'a> PairExt for Pair<'a> {
    fn first(self) -> Self {
        self.into_inner().next().unwrap()
    }

    fn first_as_string(self) -> String {
        String::from(self.first().as_str())
    }

    fn only(self) -> Pair<'a> {
        let mut iter = self.into_inner();
        let child = iter.next().unwrap();
        debug_assert_eq!(iter.next(), None);
        child
    }

    fn into_number(self) -> i32 {
        let first = self.into_inner().next().unwrap();
        match first.as_rule() {
            Rule::dec_number | Rule::dec_zero => i32::from_str(first.as_str()).unwrap(),
            Rule::hex_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 16).unwrap(),
            Rule::bin_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 2).unwrap(),
            Rule::oct_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 8).unwrap(),
            _ => panic!("Expected a number, got {}", first.as_str()),
        }
    }

    fn into_quoted_string(self) -> String {
        let mut string = String::with_capacity(self.as_str().len());
        for inner in self.into_inner() {
            let string_inner = inner.as_str();
            match string_inner {
                "\\t" => string.push('\t'),
                "\\r" => string.push('\r'),
                "\\n" => string.push('\n'),
                "\\0" => string.push('\0'),
                "\\\\" => string.push('\\'),
                "\\\"" => string.push('\"'),
                _ => string.push_str(string_inner),
            }
        }
        string
    }
}

trait PairsExt {
    fn next_if_rule(&mut self, rule: Rule) -> Option<Pair>;
    fn first(&mut self) -> Pair;
}

impl PairsExt for Peekable<Pairs<'_>> {
    fn next_if_rule(&mut self, rule: Rule) -> Option<Pair> {
        self.next_if(|p| p.as_rule() == rule)
    }

    fn first(&mut self) -> Pair {
        self.next().unwrap().first()
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ParseError(usize, usize, String);

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(err: Error<Rule>) -> Self {
        let message = err.to_string();
        match err.line_col {
            LineColLocation::Pos((line, col)) | LineColLocation::Span((line, col), _) => {
                Self(line, col, message)
            }
        }
    }
}

trait AstNode: Sized {
    const RULE: Rule;
    fn from_pair(pair: Pair) -> Self;
}

trait Parseable: Sized {
    fn from_str(src: &str) -> Result<Self, ParseError>;
}

impl<T: AstNode> Parseable for T {
    fn from_str(src: &str) -> Result<Self, ParseError> {
        let pair = ForgeParser::parse(Self::RULE, src)
            .map_err(ParseError::from)?
            .next()
            .unwrap();
        Ok(Self::from_pair(pair))
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    Program::from_str(src)
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Declaration {
    const RULE: Rule = Rule::declaration;
    fn from_pair(pair: Pair) -> Self {
        let pair = pair.first();
        match pair.as_rule() {
            Rule::function => Self::Function(Function::from_pair(pair)),
            Rule::global => Self::Global(Global::from_pair(pair)),
            Rule::struct_decl => Self::Struct(Struct::from_pair(pair)),
            Rule::const_decl => Self::Const(Const::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl Varinfo {
    fn read_or_default(mut pairs: Peekable<Pairs>) -> Self {
        pairs
            .next_if_rule(Rule::varinfo)
            .map_or(Self::default(), Self::from_pair)
    }
}

impl AstNode for Varinfo {
    const RULE: Rule = Rule::varinfo;
    fn from_pair(pair: Pair) -> Self {
        let mut children = pair.into_inner().peekable();
        let typename = children
            .next_if_rule(Rule::typename)
            .map(|t| String::from(t.first().as_str()));
        let size = children
            .next_if_rule(Rule::size)
            .map(|s| Expr::from_pair(s.first()));
        Self { typename, size }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Global {
    const RULE: Rule = Rule::global;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let Varinfo { typename, size } = Varinfo::read_or_default(inner);

        Global {
            name,
            typename,
            size,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Struct {
    const RULE: Rule = Rule::struct_decl;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let member_pairs = inner.next().unwrap().into_inner();
        let members: Vec<_> = member_pairs.map(Member::from_pair).collect();
        Struct { name, members }
    }
}

impl AstNode for Member {
    const RULE: Rule = Rule::member;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let Varinfo { typename, size } = Varinfo::read_or_default(inner);

        Member {
            name,
            typename,
            size,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Const {
    const RULE: Rule = Rule::const_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let value = inner.next().unwrap();
        match value.as_rule() {
            Rule::string => Const {
                name,
                string: Some(value.into_quoted_string()),
                value: None,
            },
            Rule::expr => Const {
                name,
                string: None,
                value: Some(Expr::from_pair(value)),
            },
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Function {
    const RULE: Rule = Rule::function;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let args: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(Argname::from_pair)
            .collect();

        let body = Block::from_pair(inner.next().unwrap());

        Self {
            name,
            args,
            body,
        }
    }
}

impl AstNode for Argname {
    const RULE: Rule = Rule::argname;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let typename = inner.next().map(|t| String::from(t.first().as_str()));
        Self { name, typename }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Statement {
    const RULE: Rule = Rule::statement;
    fn from_pair(pair: Pair) -> Self {
        let pair = pair.first();
        match pair.as_rule() {
            Rule::return_stmt => Self::Return(Return::from_pair(pair)),
            Rule::assignment => Self::Assignment(Assignment::from_pair(pair)),
            Rule::expr => Self::Expr(Expr::from_pair(pair)),
            Rule::var_decl => Self::VarDecl(VarDecl::from_pair(pair)),
            Rule::conditional => Self::Conditional(Conditional::from_pair(pair)),
            Rule::while_loop => Self::WhileLoop(WhileLoop::from_pair(pair)),
            Rule::repeat_loop => Self::RepeatLoop(RepeatLoop::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Return {
    const RULE: Rule = Rule::return_stmt;
    fn from_pair(pair: Pair) -> Self {
        pair.into_inner()
            .next()
            .map_or(Self(None), |expr| Self(Some(Expr::from_pair(expr))))
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Assignment {
    const RULE: Rule = Rule::assignment;
    fn from_pair(pair: Pair) -> Self {
        let mut pairs = pair.into_inner();
        let lvalue = Lvalue::from_pair(pairs.next().unwrap());
        let rvalue = Rvalue::from_pair(pairs.next().unwrap());
        Self { lvalue, rvalue }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Lvalue {
    const RULE: Rule = Rule::lvalue;
    fn from_pair(pair: Pair) -> Self {
        let mut pairs = pair.into_inner();
        let name = String::from(pairs.next().unwrap().as_str());
        let subscripts: Vec<_> = pairs.map(|p| match p.as_rule() {
            Rule::subscript => Suffix::Subscript(Expr::from_pair(p.first()).into()),
            Rule::member => Suffix::Member(String::from(p.as_str())),
            _ => unreachable!()
        }).collect();
        Self { name, subscripts }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Rvalue {
    const RULE: Rule = Rule::rvalue;

    fn from_pair(pair: Pair) -> Self {
        let pair = pair.first();
        match pair.as_rule() {
            Rule::string => Self::String(pair.into_quoted_string()),
            Rule::expr => Self::Expr(Expr::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for VarDecl {
    const RULE: Rule = Rule::var_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let Varinfo { typename, size } = Varinfo::from_pair(inner.next().unwrap());
        let initial = inner.next().map(Rvalue::from_pair);
        Self {
            name,
            typename,
            size,
            initial,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Block {
    const RULE: Rule = Rule::block;

    fn from_pair(pair: Pair) -> Self {
        Self(pair.into_inner().map(Statement::from_pair).collect())
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Conditional {
    const RULE: Rule = Rule::conditional;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let condition = Expr::from_pair(inner.next().unwrap());
        let body = Block::from_pair(inner.next().unwrap());
        let alternative = inner.next().map(Block::from_pair);
        Self {
            condition,
            body,
            alternative,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for WhileLoop {
    const RULE: Rule = Rule::while_loop;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        Self {
            condition: Expr::from_pair(inner.next().unwrap()),
            body: Block::from_pair(inner.next().unwrap()),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for RepeatLoop {
    const RULE: Rule = Rule::repeat_loop;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let count = Expr::from_pair(inner.next().unwrap());
        let name = inner
            .next_if_rule(Rule::name)
            .map(|p| String::from(p.as_str()));
        let body = Block::from_pair(inner.next().unwrap());
        Self { count, name, body }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Expr {
    const RULE: Rule = Rule::expr;
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
            .map_primary(|term| {
                match term.as_rule() {
                    Rule::number => Expr::Number(term.into_number()),
                    Rule::name => Expr::Name(String::from(term.as_str())),
                    Rule::expr => Expr::from_pair(term).into(),
                    _ => unreachable!()
                }
            })
            .map_infix(|lhs, op, rhs|
                Expr::Infix(lhs.into(), Operator::from_pair(op), rhs.into())
            )
            .map_prefix(|prefix, expr|
                Expr::Prefix(Prefix::from_pair(prefix), expr.into())
            )
            .map_postfix(|expr, suffix|
                Expr::Suffix(expr.into(), Suffix::from_pair(suffix)))
            .parse(pair.into_inner())
    }
}

impl Expr {
    pub(crate) fn parse(src: &str) -> Result<Self, ParseError> {
        Self::from_str(src)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Operator {
    const RULE: Rule = Rule::operator;
    fn from_pair(pair: Pair) -> Self {
        match pair.as_str() {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Mod,
            "&&" => Self::And,
            "||" => Self::Or,
            "&" => Self::BitAnd,
            "|" => Self::BitOr,
            "^" => Self::Xor,
            ">" => Self::Gt,
            ">=" => Self::Ge,
            "<" => Self::Lt,
            "<=" => Self::Le,
            "==" => Self::Eq,
            "!=" => Self::Ne,
            "<<" => Self::Lshift,
            ">>" => Self::Rshift,
            _ => unreachable!(),
        }
    }
}

impl AstNode for Prefix {
    const RULE: Rule = Rule::prefix;

    fn from_pair(pair: Pair) -> Self {
        match pair.as_str() {
            "!" => Self::Not,
            "-" => Self::Neg,
            "&" => Self::Address,
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Suffix {
    const RULE: Rule = Rule::suffix;

    fn from_pair(pair: Pair) -> Self {
        let first = pair.first();
        match first.as_rule() {
            Rule::subscript => Self::Subscript(Expr::from_pair(first.first()).into()),
            Rule::member => Self::Member(first.first_as_string()),
            Rule::arglist => Self::Arglist(first.into_inner().map(Rvalue::from_pair).collect()),
            rule => unreachable!("Expected a subscript, member, or arglist, got a {:?}", rule)
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Program {
    const RULE: Rule = Rule::program;
    fn from_pair(pair: Pair) -> Self {
        // Program captures EOI, to make sure that it's parsing the entire stream. We need to
        // ignore that though:
        Self(
            pair.into_inner()
                .filter(|p| p.as_rule() != Rule::EOI)
                .map(Declaration::from_pair)
                .collect(),
        )
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_globals() {
        assert_eq!(
            Global::from_str("global foo;"),
            Ok(Global {
                name: "foo".into(),
                typename: None,
                size: None,
            })
        );
        assert_eq!(
            Global::from_str("global foo:Thing;"),
            Ok(Global {
                name: "foo".into(),
                typename: Some("Thing".into()),
                size: None,
            })
        );
        assert_eq!(
            Global::from_str("global foo:Thing[10];"),
            Ok(Global {
                name: "foo".into(),
                typename: Some("Thing".into()),
                size: Some(10.into()),
            })
        );
        assert_eq!(
            Global::from_str("global foo[10];"),
            Ok(Global {
                name: "foo".into(),
                typename: None,
                size: Some(10.into()),
            })
        );
    }

    #[test]
    fn parse_consts() {
        assert_eq!(
            Const::from_str("const a = 123;"),
            Ok(Const {
                name: "a".into(),
                value: Some(123.into()),
                string: None,
            })
        );
        assert_eq!(
            Const::from_str("const a = 0xaa;"),
            Ok(Const {
                name: "a".into(),
                value: Some(0xaa.into()),
                string: None,
            })
        );
        assert_eq!(
            Const::from_str("const a = -7;"),
            Ok(Const {
                name: "a".into(),
                value: Some(Expr::Prefix(Prefix::Neg, 7.into()).into()),
                string: None,
            })
        );
        assert_eq!(
            Const::from_str("const a = \"foo bar\";"),
            Ok(Const {
                name: "a".into(),
                value: None,
                string: Some("foo bar".into()),
            })
        )
    }

    #[test]
    fn parse_structs() {
        assert_eq!(
            Struct::from_str("struct Point { x, y }"),
            Ok(Struct {
                name: "Point".into(),
                members: vec![
                    Member {
                        name: "x".into(),
                        typename: None,
                        size: None,
                    },
                    Member {
                        name: "y".into(),
                        typename: None,
                        size: None,
                    },
                ],
            })
        );

        assert_eq!(
            Struct::from_str("struct Foo { bar[100] }"),
            Ok(Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: None,
                    size: Some(100.into()),
                }, ],
            })
        );

        assert_eq!(
            Struct::from_str("struct Foo { bar:Thing[100] }"),
            Ok(Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: Some("Thing".into()),
                    size: Some(100.into()),
                }, ],
            })
        );
    }

    #[test]
    fn parse_function_headers() {
        assert_eq!(
            Function::from_str("fn foo() {}"),
            Ok(Function {
                name: "foo".into(),
                args: vec![],
                body: Block(vec![]),
            })
        );
        assert_eq!(
            Function::from_str("fn foo(a, b) {}"),
            Ok(Function {
                name: "foo".into(),
                args: vec![
                    Argname {
                        name: "a".into(),
                        typename: None,
                    },
                    Argname {
                        name: "b".into(),
                        typename: None,
                    },
                ],
                body: Block(vec![]),
            })
        );
        assert_eq!(
            Function::from_str("fn foo(a:Blah, b) {}"),
            Ok(Function {
                name: "foo".into(),
                args: vec![
                    Argname {
                        name: "a".into(),
                        typename: Some("Blah".into()),
                    },
                    Argname {
                        name: "b".into(),
                        typename: None,
                    },
                ],
                body: Block(vec![]),
            })
        );

        let func = Function {
            name: "foo".into(),
            args: vec![Argname {
                name: "a".into(),
                typename: None,
            }],
            body: Block(vec![]),
        };
    }

    #[test]
    fn parse_exprs() {
        use Operator::*;
        use Prefix::*;
        use Suffix::*;
        use Expr::Infix;

        // A very, very basic expression
        assert_eq!(
            Expr::from_str("23"),
            Ok(Expr::Number(23))
        );

        // Two vals with an operator
        assert_eq!(
            Expr::from_str("23 + 5"),
            Ok(Infix(23.into(), Add, 5.into()))
        );

        // Multiple terms at the same precedence level
        assert_eq!(
            Expr::from_str("1 + 2 + 3"),
            Ok(Infix(Infix(1.into(), Add, 2.into()).into(), Add, 3.into()))
        );

        // Simple prefix
        assert_eq!(Expr::from_str("-5"),
            Ok(Expr::Prefix(Neg, 5.into())));

        // Multiple prefixes
        assert_eq!(
            Expr::from_str("!&foo"),
            Ok(Expr::Prefix(Not,
                            Expr::Prefix(Address, "foo".into()).into())));

        // Simple suffix
        assert_eq!(
            Expr::from_str("foo[10]"),
            Ok(Expr::Suffix("foo".into(), Subscript(10.into())))
        );

        // Multi-suffix
        assert_eq!(
            Expr::from_str("foo[10].bar"),
            Ok(Expr::Suffix(
                Expr::Suffix("foo".into(), Subscript(10.into())).into(),
                Member("bar".into())))
        );

        // Higher precedence levels
        assert_eq!(
            Expr::from_str("1 + 2 * 3"),
            Ok(Infix(1.into(), Add, Infix(2.into(), Mul, 3.into()).into()))
        );

        assert_eq!(
            Expr::from_str("2 * 3"),
            Ok(Infix(2.into(), Mul, 3.into()))
        );

        assert_eq!(
            Expr::from_str("2 * 3 + 4"),
            Ok(Infix(Infix(2.into(), Mul, 3.into()).into(), Add, 4.into()))
        );

        // Various operators
        assert_eq!(
            Expr::from_str("1 || 2 && 3"),
            Ok(Infix(1.into(), Or, Infix(2.into(), And, 3.into()).into()))
        );

        assert_eq!(
            Expr::from_str("2 && &blah"),
            Ok(Infix(2.into(), And, Expr::Prefix(Address, "blah".into()).into()))
        );

        assert_eq!(
            Expr::from_str("2 & &blah"),
            Ok(Infix(2.into(), BitAnd, Expr::Prefix(Address, "blah".into()).into()))
        );

        assert_eq!(
            Expr::from_str("1 | 2 ^ 3"),
            Ok(Infix(1.into(), BitOr, Infix(2.into(), Xor, 3.into()).into()))
        );

        assert_eq!(
            Expr::from_str("x == y > z"),
            Ok(Infix("x".into(), Eq, Infix("y".into(), Gt, "z".into()).into()))
        );

        assert_eq!(
            Expr::from_str("1 << 6"),
            Ok(Infix(1.into(), Lshift, 6.into()))
        );

        assert_eq!(
            Expr::from_str("!a - -3"),
            Ok(Infix(Expr::Prefix(Not, "a".into()).into(), Sub, Expr::Prefix(Neg, 3.into()).into()))
        );

        assert_eq!(
            Expr::from_str("-(4 * 5)"),
            Ok(Expr::Prefix(Neg,
                            Infix(4.into(), Mul, 5.into()).into()
            ))
        );

        // Parens
        assert_eq!(
            Expr::from_str("(1 + 2) * 3"),
            Ok(
                Infix(
                    Infix(1.into(), Add, 2.into()).into(),
                    Mul,
                    3.into()
                )
            )
        );
    }

    #[test]
    fn parse_calls() {
        let blah = Expr::Suffix(
            "blah".into(),
            Suffix::Arglist(vec![])
        );

        // Can Node parse a call?
        assert_eq!(Expr::from_str("blah()"), Ok(blah.clone()));

        // Can Statement parse a call?
        assert_eq!(Statement::from_str("blah();"), Ok(Statement::Expr(blah)));

        // Calls with args
        assert_eq!(
            Expr::from_str("blah(1, 2)"),
            Ok(Expr::Suffix(
                "blah".into(),
                Suffix::Arglist(vec![1.into(), 2.into()])))
        );

        //Calls with strings
        assert_eq!(
            Expr::from_str("blah(\"foo\", 2)"),
            Ok(Expr::Suffix(
                "blah".into(),
                Suffix::Arglist(vec!["foo".into(), 2.into()])))
        );
     }

    #[test]
    fn parse_return() {
        assert_eq!(
            Statement::from_str("return;"),
            Ok(Statement::Return(Return(None)))
        );

        assert_eq!(
            Statement::from_str("return 17;"),
            Ok(Statement::Return(Return(Some(17.into()))))
        );
    }

    #[test]
    fn parse_assignment() {
        assert_eq!(
            Statement::from_str("foo = 7;"),
            Ok(Statement::Assignment(Assignment {
                lvalue: "foo".into(),
                rvalue: Rvalue::Expr(7.into()),
            }))
        );

        assert_eq!(
            Assignment::from_str("foo[45] = 7"),
            Ok(Assignment {
                lvalue: Lvalue { name: String::from("foo"), subscripts: vec![Suffix::Subscript(45.into())] },
                rvalue: Rvalue::Expr(7.into()),
            })
        );
    }

    #[test]
    fn parse_var_decl() {
        assert_eq!(
            Statement::from_str("var blah;"),
            Ok(Statement::VarDecl(VarDecl {
                name: "blah".into(),
                typename: None,
                size: None,
                initial: None,
            }))
        );

        assert_eq!(
            VarDecl::from_str("var blah:Foo[7] = 35"),
            Ok(VarDecl {
                name: "blah".into(),
                typename: Some("Foo".into()),
                size: Some(7.into()),
                initial: Some(35.into()),
            })
        );
    }

    #[test]
    fn parse_block() {
        assert_eq!(
            Block::from_str("{ foo(); bar(); }"),
            Ok(Block(vec![
                Statement::Expr(Expr::Suffix("foo".into(), Suffix::Arglist(vec![]))),
                Statement::Expr(Expr::Suffix("bar".into(), Suffix::Arglist(vec![]))),
            ]))
        );
    }

    #[test]
    fn parse_conditional() {
        assert_eq!(
            Statement::from_str("if(cond) { foo(); }"),
            Ok(Statement::Conditional(Conditional {
                condition: Expr::from_str("cond").unwrap(),
                body: Block::from_str("{ foo(); }").unwrap(),
                alternative: None,
            }))
        );

        assert_eq!(
            Statement::from_str("if(cond) { foo(); } else { bar(); }"),
            Ok(Statement::Conditional(Conditional {
                condition: Expr::from_str("cond").unwrap(),
                body: Block::from_str("{ foo(); }").unwrap(),
                alternative: Some(Block::from_str("{ bar(); }").unwrap()),
            }))
        );
    }

    #[test]
    fn parse_while_loops() {
        assert_eq!(
            Statement::from_str("while(cond) { foo(); }"),
            Ok(Statement::WhileLoop(WhileLoop {
                condition: Expr::from_str("cond").unwrap(),
                body: Block::from_str("{ foo(); }").unwrap(),
            }))
        );
    }

    #[test]
    fn parse_repeat_loops() {
        assert_eq!(
            Statement::from_str("repeat(10) x { foo(x); }"),
            Ok(Statement::RepeatLoop(RepeatLoop {
                count: 10.into(),
                name: Some("x".into()),
                body: Block::from_str("{ foo(x); }").unwrap(),
            }))
        );

        assert_eq!(
            Statement::from_str("repeat(10) { foo(); }"),
            Ok(Statement::RepeatLoop(RepeatLoop {
                count: 10.into(),
                name: None,
                body: Block::from_str("{ foo(); }").unwrap(),
            }))
        );
    }

    #[test]
    fn parse_program() {
        assert_eq!(
            Program::from_str("global foo; struct Point { x, y }"),
            Ok(Program(vec![
                Declaration::from_str("global foo;").unwrap(),
                Declaration::from_str("struct Point { x, y }").unwrap(),
            ]))
        )
    }
}
