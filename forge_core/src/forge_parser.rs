use pest::{Parser, RuleType};

mod inner {
    #[derive(Parser)]
    #[grammar = "forge.pest"]
    pub struct ForgeParser;
}

use crate::ast::*;
use inner::*;
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
            Rule::dec_number | Rule::dec_zero | Rule::neg_number => {
                i32::from_str(first.as_str()).unwrap()
            }
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
    fn parse(src: &str) -> Result<Self, ParseError>;
}

impl<T: AstNode> Parseable for T {
    fn parse(src: &str) -> Result<Self, ParseError> {
        let pair = ForgeParser::parse(Self::RULE, src)
            .map_err(ParseError::from)?
            .next()
            .unwrap();
        Ok(Self::from_pair(pair))
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Declaration {
    const RULE: Rule = Rule::declaration;
    fn from_pair(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::function => Self::Function(Function::from_pair(pair)),
            Rule::global => Self::Global(Global::from_pair(pair)),
            Rule::struct_decl => Self::Struct(Struct::from_pair(pair)),
            Rule::const_decl => Self::Const(Const::from_pair(pair)),
            _ => todo!(),
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
            .map(|s| s.first().into_number());
        Self { typename, size }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Global {
    const RULE: Rule = Rule::global;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let varinfo = Varinfo::read_or_default(inner);

        Global {
            name,
            typename: varinfo.typename,
            size: varinfo.size,
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
        let varinfo = Varinfo::read_or_default(inner);

        Member {
            name,
            typename: varinfo.typename,
            size: varinfo.size,
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
                string: Some(String::from(value.into_quoted_string())),
                value: None,
            },
            Rule::number => Const {
                name,
                string: None,
                value: Some(value.into_number()),
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
        let mut inline = false;
        let mut org = None;
        let mut typename = None;
        if let Some(annotations) = inner.next_if_rule(Rule::annotations) {
            for annotation in annotations.into_inner() {
                let annotation = annotation.first();
                match annotation.as_rule() {
                    Rule::inline_annotation => inline = true,
                    Rule::org_annotation => org = Some(annotation.first().into_number()),
                    Rule::type_annotation => typename = Some(annotation.first_as_string()),
                    _ => unreachable!(),
                }
            }
        }
        let args: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(Argname::from_pair)
            .collect();

        Self {
            name,
            org,
            typename,
            inline,
            args,
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
    const RULE: Rule = Rule::declaration;
    fn from_pair(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::return_stmt => Self::Return(Return::from_pair(pair)),
            Rule::assignment => Self::Assignment(Assignment::from_pair(pair)),
            Rule::call => Self::Call(Call::from_pair(pair)),
            Rule::var_decl => Self::VarDecl(VarDecl::from_pair(pair)),
            Rule::conditional => Self::Conditional(Conditional::from_pair(pair)),
            Rule::while_loop => Self::WhileLoop(WhileLoop::from_pair(pair)),
            Rule::repeat_loop => Self::RepeatLoop(RepeatLoop::from_pair(pair)),
            _ => todo!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Return {
    const RULE: Rule = Rule::return_stmt;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Assignment {
    const RULE: Rule = Rule::assignment;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Call {
    const RULE: Rule = Rule::call;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for VarDecl {
    const RULE: Rule = Rule::var_decl;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Conditional {
    const RULE: Rule = Rule::conditional;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for WhileLoop {
    const RULE: Rule = Rule::while_loop;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for RepeatLoop {
    const RULE: Rule = Rule::repeat_loop;
    fn from_pair(pair: Pair) -> Self {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Node {
    const RULE: Rule = Rule::expr; // Also used for vals / terms / the whole tree
    fn from_pair(pair: Pair) -> Self {
        let expr = match pair.as_rule() {
            Rule::val => Node::from_pair(pair.first()),
            Rule::number => Node::Number(pair.into_number()),
            Rule::name => Node::Name(String::from(pair.as_str())),
            Rule::call => todo!(),
            Rule::arrayref => todo!(),
            Rule::address => todo!(),
            Rule::expr | Rule::term => {
                let mut children = pair.into_inner();
                let car = Node::from_pair(children.next().unwrap());
                Node::Expr(car.into(), parse_expr(children))
            }
            _ => unreachable!(),
        };
        shake_expr(expr)
    }
}

fn parse_expr(mut cdr: Pairs) -> Vec<(Operator, Node)> {
    let mut terms = Vec::new();
    while let Some(operator) = cdr.next() {
        let rhs = cdr.next().unwrap();
        let rhs_r = rhs.as_rule();
        let rhs = Node::from_pair(rhs);
        let op = Operator::from_pair(operator);
        terms.push((op, rhs));
    }
    terms
}

fn shake_expr(node: Node) -> Node {
    match node {
        Node::Expr(car, cdr) => {
            let shaken_car = shake_expr(car.into());
            if cdr.is_empty() {
                shaken_car
            } else {
                let shaken_cdr = cdr.into_iter().map(|(op, n)| (op, shake_expr(n))).collect();
                Node::Expr(shaken_car.into(), shaken_cdr)
            }
        }
        _ => node,
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Operator {
    const RULE: Rule = Rule::sign; // also term_op
    fn from_pair(pair: Pair) -> Self {
        match pair.as_str() {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Mod,
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_globals() {
        assert_eq!(
            Global::parse("global foo;"),
            Ok(Global {
                name: "foo".into(),
                typename: None,
                size: None
            })
        );
        assert_eq!(
            Global::parse("global foo:Thing;"),
            Ok(Global {
                name: "foo".into(),
                typename: Some("Thing".into()),
                size: None
            })
        );
        assert_eq!(
            Global::parse("global foo:Thing[10];"),
            Ok(Global {
                name: "foo".into(),
                typename: Some("Thing".into()),
                size: Some(10)
            })
        );
        assert_eq!(
            Global::parse("global foo[10];"),
            Ok(Global {
                name: "foo".into(),
                typename: None,
                size: Some(10)
            })
        );
    }

    #[test]
    fn parse_consts() {
        assert_eq!(
            Const::parse("const a = 123;"),
            Ok(Const {
                name: "a".into(),
                value: Some(123),
                string: None
            })
        );
        assert_eq!(
            Const::parse("const a = 0xaa;"),
            Ok(Const {
                name: "a".into(),
                value: Some(0xaa),
                string: None
            })
        );
        assert_eq!(
            Const::parse("const a = -7;"),
            Ok(Const {
                name: "a".into(),
                value: Some(-7),
                string: None
            })
        );
        assert_eq!(
            Const::parse("const a = \"foo bar\";"),
            Ok(Const {
                name: "a".into(),
                value: None,
                string: Some("foo bar".into())
            })
        )
    }

    #[test]
    fn parse_structs() {
        assert_eq!(
            Struct::parse("struct Point { x, y }"),
            Ok(Struct {
                name: "Point".into(),
                members: vec![
                    Member {
                        name: "x".into(),
                        typename: None,
                        size: None
                    },
                    Member {
                        name: "y".into(),
                        typename: None,
                        size: None
                    },
                ]
            })
        );

        assert_eq!(
            Struct::parse("struct Foo { bar[100] }"),
            Ok(Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: None,
                    size: Some(100)
                },]
            })
        );

        assert_eq!(
            Struct::parse("struct Foo { bar:Thing[100] }"),
            Ok(Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: Some("Thing".into()),
                    size: Some(100)
                },]
            })
        );
    }

    #[test]
    fn parse_function_headers() {
        assert_eq!(
            Function::parse("fn foo() {}"),
            Ok(Function {
                name: "foo".into(),
                inline: false,
                org: None,
                typename: None,
                args: vec![],
            })
        );
        assert_eq!(
            Function::parse("fn foo(a, b) {}"),
            Ok(Function {
                name: "foo".into(),
                inline: false,
                org: None,
                typename: None,
                args: vec![
                    Argname {
                        name: "a".into(),
                        typename: None
                    },
                    Argname {
                        name: "b".into(),
                        typename: None
                    }
                ],
            })
        );
        assert_eq!(
            Function::parse("fn foo(a:Blah, b) {}"),
            Ok(Function {
                name: "foo".into(),
                inline: false,
                org: None,
                typename: None,
                args: vec![
                    Argname {
                        name: "a".into(),
                        typename: Some("Blah".into())
                    },
                    Argname {
                        name: "b".into(),
                        typename: None
                    }
                ],
            })
        );
        assert_eq!(
            Function::parse("fn foo<inline, org=0x400>(a) {}"),
            Ok(Function {
                name: "foo".into(),
                inline: true,
                org: Some(0x400),
                typename: None,
                args: vec![Argname {
                    name: "a".into(),
                    typename: None
                },],
            })
        );
        assert_eq!(
            Function::parse("fn foo<org=0x400, inline>(a) {}"),
            Ok(Function {
                name: "foo".into(),
                inline: true,
                org: Some(0x400),
                typename: None,
                args: vec![Argname {
                    name: "a".into(),
                    typename: None
                },],
            })
        );
    }

    #[test]
    fn parse_exprs() {
        use Node::*;
        use Operator::*;
        // A very, very basic expression
        assert_eq!(Node::parse("23"), Ok(Number(23)));

        // Two vals with an operator
        assert_eq!(
            Node::parse("23 + 5"),
            Ok(Expr(23.into(), vec![(Add, Number(5))]))
        );

        // Multiple terms at the same precedence level
        assert_eq!(
            Node::parse("1 + 2 + 3"),
            Ok(Expr(1.into(), vec![(Add, 2.into()), (Add, 3.into())]))
        );

        // Higher precedence levels
        assert_eq!(
            Node::parse("1 + 2 * 3"),
            Ok(Node::Expr(
                1.into(),
                vec![(Add, Expr(2.into(), vec![(Mul, 3.into())]))]
            ))
        );
        assert_eq!(
            Node::parse("2 * 3"),
            Ok(Expr(Number(2).into(), vec![(Mul, 3.into())]))
        );
        assert_eq!(
            Node::parse("2 * 3 + 4"),
            Ok(Expr(
                Expr(2.into(), vec![(Mul, 3.into())]).into(),
                vec![(Add, 4.into())]
            ))
        );

        // Parens
        assert_eq!(
            Node::parse("(1 + 2) * 3"),
            Ok(Node::Expr(
                Expr(1.into(), vec![(Add, 2.into())]).into(),
                vec![(Mul, 3.into())]
            ))
        );
    }
}
