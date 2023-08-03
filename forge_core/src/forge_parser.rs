use pest::pratt_parser::PrattParser;
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
    };
}

use crate::ast::*;
use pest::error::{Error, LineColLocation};
use std::iter::Peekable;
use std::str::FromStr;
use crate::ast::Suffix::{Arglist, Subscript};

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
        let lvalue_pair = pairs.next().unwrap().first();
        let lvalue = match lvalue_pair.as_rule() {
            //Rule::arrayref => Lvalue::ArrayRef(ArrayRef::from_pair(lvalue_pair)),
            Rule::name => Lvalue::Name(String::from(lvalue_pair.as_str())),
            _ => unreachable!(),
        };
        let rvalue = Rvalue::from_pair(pairs.next().unwrap());
        Self { lvalue, rvalue }
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
        let initial = inner.next().map(Expr::from_pair);
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
        PRATT_PARSER
            .map_primary(|val| {
                    Expr {
                        lhs: Val::from_pair(val),
                        op: None,
                        rhs: None
                    }
            })
            .map_infix(|lhs, op, rhs|
                Expr {
                    lhs: Val::from(lhs),
                    op: Some(Operator::from_pair(op)),
                    rhs: Some(rhs.into()),
                })
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
    // also term_op
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
            Rule::subscript => Subscript(Expr::from_pair(first.first())),
            Rule::member => Self::Member(first.first_as_string()),
            Rule::arglist => Arglist(first.into_inner().map(Rvalue::from_pair).collect()),
            _ => unreachable!()
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

impl AstNode for Val {
    const RULE: Rule = Rule::val;

    fn from_pair(pair: Pair) -> Self {
        let mut suffix = vec![];
        let mut prefix = vec![];
        let mut val = None;
        for child in pair.into_inner() {
            match child.as_rule() {
                Rule::prefix => prefix.push(Prefix::from_pair(child)),
                Rule::suffix => suffix.push(Suffix::from_pair(child)),
                Rule::number | Rule::name | Rule::expr => val = Some(child),
                _ => unreachable!()
            }
        }

        let val = val.unwrap();
        match val.as_rule() {
            Rule::number => Self::Number(val.into_number(), prefix, suffix),
            Rule::name => Self::Name(String::from(val.as_str()), prefix, suffix),
            Rule::expr => Self::Expr(Expr::from_pair(val).into(), prefix, suffix),
            _ => unreachable!()
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_vals() {
        // Basic vals with no extras:
        assert_eq!(Val::from_str("10"), Ok(10.into()));
        assert_eq!(Val::from_str("blah"), Ok(Val::Name("blah".into(), vec![], vec![])));

        // Simple prefix
        assert_eq!(Val::from_str("-5"),
                   Ok(Val::Number(5, vec![Prefix::Neg], vec![])));

        // Multiple prefixes
        assert_eq!(Val::from_str("!&foo"),
                   Ok(Val::Name("foo".into(), vec![Prefix::Not, Prefix::Address], vec![])));

        // Simple suffix
        assert_eq!(Val::from_str("foo[10]"),
                   Ok(Val::Name("foo".into(), vec![], vec![Suffix::Subscript(10.into())])));

        // Multi-suffix
        assert_eq!(Val::from_str("foo[10].bar"),
                   Ok(Val::Name("foo".into(), vec![], vec![Suffix::Subscript(10.into()), Suffix::Member("bar".into())])));
    }

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
                value: Some(Val::Number(7, vec![Prefix::Neg], vec![]).into()),
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

        assert_eq!(
            Function::from_str("fn foo<inline, org=0x400>(a) {}"),
            Ok(func.clone())
        );
        assert_eq!(
            Function::from_str("fn foo<org=0x400, inline>(a) {}"),
            Ok(func)
        );
    }

    #[test]
    fn parse_exprs() {
        use Operator::*;
        // A very, very basic expression
        // For the rest of these we'll .into() stuff for brevity
        //Some(assert_eq!(Expr::from_str("23"), Ok(Expr { lhs: Val::Number(23, vec![], vec![]), op: None, rhs: None })));

        // Two vals with an operator
        assert_eq!(
            Expr::from_str("23 + 5"),
            Ok(Expr { lhs: 23.into(), op: Some(Add), rhs: Some(5.into()) })
        );

        // Multiple terms at the same precedence level
        assert_eq!(
            Expr::from_str("1 + 2 + 3"),
            Ok(Expr { lhs: 1.into(), op: Some(Add), rhs: Some(Expr { lhs: 2.into(), op: Some(Add), rhs: Some(3.into()) }.into()) })
        );

        /*
                // Higher precedence levels
                assert_eq!(
                    Node::from_str("1 + 2 * 3"),
                    Ok(Expr(1.into(), Add, Expr(2.into(), Mul, 3.into()).into()))
                );

                assert_eq!(Node::from_str("2 * 3"), Ok(Expr(2.into(), Mul, 3.into())));

                assert_eq!(
                    Node::from_str("2 * 3 + 4"),
                    Ok(Expr(Expr(2.into(), Mul, 3.into()).into(), Add, 4.into()))
                );

                // Various operators
                assert_eq!(
                    Node::from_str("1 || 2 && 3"),
                    Ok(Expr(1.into(), Or, Expr(2.into(), And, 3.into()).into()))
                );

                assert_eq!(
                    Node::from_str("2 && &blah"),
                    Ok(Expr(2.into(), And, Address("blah".into()).into()))
                );

                assert_eq!(
                    Node::from_str("2 & &blah"),
                    Ok(Expr(2.into(), BitAnd, Address("blah".into()).into()))
                );

                assert_eq!(
                    Node::from_str("1 | 2 ^ 3"),
                    Ok(Expr(1.into(), BitOr, Expr(2.into(), Xor, 3.into()).into()))
                );

                assert_eq!(
                    Node::from_str("x == y > z"),
                    Ok(Expr(
                        Name("x".into()).into(),
                        Eq,
                        Expr(Name("y".into()).into(), Gt, Name("z".into()).into()).into()
                    ))
                );

                assert_eq!(
                    Node::from_str("1 << 6"),
                    Ok(Expr(1.into(), Lshift, 6.into()))
                );

                assert_eq!(
                    Node::from_str("!a - -3"),
                    Ok(Expr(
                        Prefix(crate::ast::Prefix::Not, Name("a".into()).into()).into(),
                        Sub,
                        Prefix(crate::ast::Prefix::Neg, 3.into()).into()
                    ))
                );

                assert_eq!(
                    Node::from_str("-(4 * 5)"),
                    Ok(Prefix(
                        crate::ast::Prefix::Neg,
                        Expr(4.into(), Mul, 5.into()).into()
                    ))
                );

                // Parens
                assert_eq!(
                    Node::from_str("(1 + 2) * 3"),
                    Ok(Expr(Expr(1.into(), Add, 2.into()).into(), Mul, 3.into()))
                );
               */
    }

    #[test]
    fn parse_arrayrefs() {
        // use crate::ast::ArrayRef as AR;
        // use Val::*;
        // use Operator::*;
        //
        // // Normal numbers
        // assert_eq!(
        //     AR::from_str("foo[7]"),
        //     Ok(AR {
        //         name: "foo".into(),
        //         subscript: Number(7).into()
        //     })
        // );
        //
        // // Full exprs (this is the last one of these; the full expr test above covers it
        // assert_eq!(
        //     AR::from_str("foo[7+x]"),
        //     Ok(AR {
        //         name: "foo".into(),
        //         subscript: Node::from_str("7+x").unwrap().into()
        //     })
        // );

        // Exprs that are actually arrayrefs
        // assert_eq!(
        //     Node::from_str("foo[7]"),
        //     Ok(ArrayRef(AR {
        //         name: "foo".into(),
        //         subscript: Number(7).into()
        //     }))
        // );
    }

    #[test]
    fn parse_addresses() {
        // assert_eq!(Node::from_str("&foo"), Ok(Node::Address("foo".into())));
    }

    // #[test]
    // fn parse_calls() {
    //     use Val::Number;
    //
    //     let blah = Call {
    //         name: "blah".into(),
    //         args: vec![],
    //     };
    //
    //     // Can Node parse a call?
    //     assert_eq!(Node::from_str("blah()"), Ok(Node::Call(blah.clone())));
    //
    //     // Can Statement parse a call?
    //     assert_eq!(Statement::from_str("blah();"), Ok(Statement::Call(blah)));
    //
    //     // Calls with args
    //     // assert_eq!(
    //     //     Call::from_str("blah(1, 2)"),
    //     //     Ok(Call {
    //     //         name: "blah".into(),
    //     //         args: vec![Rvalue::Expr(Number(1)), Rvalue::Expr(Number(2))]
    //     //     })
    //     // );
    //
    //     // Calls with strings
    //     // assert_eq!(
    //     //     Call::from_str("blah(\"foo\", 2)"),
    //     //     Ok(Call {
    //     //         name: "blah".into(),
    //     //         args: vec![Rvalue::String("foo".into()), Rvalue::Expr(Number(2))]
    //     //     })
    //     // );
    // }

    #[test]
    fn parse_return() {
        // assert_eq!(
        //     Statement::from_str("return;"),
        //     Ok(Statement::Return(Return(None)))
        // );
        //
        // assert_eq!(
        //     Statement::from_str("return 17;"),
        //     Ok(Statement::Return(Return(Some(Node::Number(17)))))
        // );
    }

    #[test]
    fn parse_assignment() {
        assert_eq!(
            Statement::from_str("foo = 7;"),
            Ok(Statement::Assignment(Assignment {
                lvalue: Lvalue::Name("foo".into()),
                rvalue: Rvalue::Expr(7.into()),
            }))
        );

        assert_eq!(
            Assignment::from_str("foo[45] = 7"),
            Ok(Assignment {
                lvalue: Lvalue::ArrayRef(ArrayRef {
                    name: "foo".into(),
                    subscript: 45.into(),
                }),
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

    // #[test]
    // fn parse_block() {
    //     assert_eq!(
    //         Block::from_str("{ foo(); bar(); }"),
    //         Ok(Block(vec![
    //             Statement::Call(Call {
    //                 name: "foo".into(),
    //                 args: vec![]
    //             }),
    //             Statement::Call(Call {
    //                 name: "bar".into(),
    //                 args: vec![]
    //             }),
    //         ]))
    //     );
    // }

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
        // assert_eq!(
        //     Statement::from_str("repeat(10) x { foo(x); }"),
        //     Ok(Statement::RepeatLoop(RepeatLoop {
        //         count: Node::Number(10),
        //         name: Some("x".into()),
        //         body: Block::from_str("{ foo(x); }").unwrap(),
        //     }))
        // );
        //
        // assert_eq!(
        //     Statement::from_str("repeat(10) { foo(); }"),
        //     Ok(Statement::RepeatLoop(RepeatLoop {
        //         count: Node::Number(10),
        //         name: None,
        //         body: Block::from_str("{ foo(); }").unwrap(),
        //     }))
        // );
    }

    #[test]
    fn parse_program() {
        // assert_eq!(
        //     Program::from_str("global foo; struct Point { x, y }"),
        //     Ok(Program(vec![
        //         Declaration::from_str("global foo;").unwrap(),
        //         Declaration::from_str("struct Point { x, y }").unwrap(),
        //     ]))
        // )
    }
}
