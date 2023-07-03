use pest::{Parser, RuleType};

mod inner {
    #[derive(Parser)]
    #[grammar = "forge.pest"]
    pub struct ForgeParser;
}

use inner::*;
use pest::error::{Error, LineColLocation};
use std::iter::Peekable;
use std::str::FromStr;

pub(crate) type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub(crate) type Pairs<'i, R = Rule> = pest::iterators::Pairs<'i, R>;

trait Children {
    fn first(self) -> Self;
    fn only(self) -> Self;
}

impl<'a> Children for Pair<'a> {
    fn first(self) -> Self {
        self.into_inner().next().unwrap()
    }

    fn only(self) -> Pair<'a> {
        let mut iter = self.into_inner();
        let child = iter.next().unwrap();
        debug_assert_eq!(iter.next(), None);
        child
    }
}

trait PairsExt {
    fn next_if_rule(&mut self, rule: Rule) -> Option<Pair>;
}

impl PairsExt for Peekable<Pairs<'_>> {
    fn next_if_rule(&mut self, rule: Rule) -> Option<Pair> {
        self.next_if(|p| p.as_rule() == rule)
    }
}

fn parse_number(pair: Pair) -> i32 {
    let first = pair.into_inner().next().unwrap();
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

/// Create a String containing the string represented by a given pair. Since the pair will
/// reference bytes containing escape sequences, this isn't the same as an &str to the
/// original code; this is a new string translating those escape sequences to their actual
/// bytes.
fn parse_string(pair: Pair) -> String {
    let mut string = String::with_capacity(pair.as_str().len());
    for inner in pair.into_inner() {
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

fn parse_name(pair: Pair) -> &str {
    pair.as_str()
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

trait Parseable<'a>: From<Pair<'a>> {
    const RULE: Rule;
    fn parse(src: &'a str) -> Result<Self, ParseError> {
        let pair = ForgeParser::parse(Self::RULE, src)
            .map_err(ParseError::from)?
            .next()
            .unwrap();
        Ok(pair.into())
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Declaration {
    Function(Function),
    Global(Global),
    Struct(Struct),
    Const(Const),
}

impl<'a> From<Pair<'a>> for Declaration {
    fn from(pair: Pair) -> Self {
        match pair.as_rule() {
            Rule::function => todo!(),
            Rule::global => Self::Global(pair.into()),
            Rule::struct_decl => Self::Struct(pair.into()),
            Rule::const_decl => Self::Const(pair.into()),
            _ => todo!(),
        }
    }
}

impl Parseable<'_> for Declaration {
    const RULE: Rule = Rule::declaration;
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub struct Varinfo {
    typename: Option<String>,
    size: Option<i32>,
}

impl Varinfo {
    fn read_or_default(mut pairs: Peekable<Pairs>) -> Self {
        pairs
            .next_if_rule(Rule::varinfo)
            .map_or(Self::default(), |v| v.into())
    }
}

impl From<Pair<'_>> for Varinfo {
    fn from(pair: Pair<'_>) -> Self {
        let mut children = pair.into_inner().peekable();
        let typename = children
            .next_if_rule(Rule::typename)
            .map(|t| String::from(t.first().as_str()));
        let size = children
            .next_if_rule(Rule::size)
            .map(|s| parse_number(s.first()));
        Self { typename, size }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Global {
    name: String,
    typename: Option<String>,
    size: Option<i32>,
}

impl From<Pair<'_>> for Global {
    fn from(global: Pair) -> Self {
        let mut inner = global.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let varinfo = Varinfo::read_or_default(inner);

        Global {
            name,
            typename: varinfo.typename,
            size: varinfo.size,
        }
    }
}

impl Parseable<'_> for Global {
    const RULE: Rule = Rule::global;
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Struct {
    name: String,
    members: Vec<Member>,
}

impl From<Pair<'_>> for Struct {
    fn from(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let member_pairs = inner.next().unwrap().into_inner();
        let members: Vec<_> = member_pairs.map(|member| member.into()).collect();
        Struct { name, members }
    }
}

impl Parseable<'_> for Struct {
    const RULE: Rule = Rule::struct_decl;
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Member {
    name: String,
    typename: Option<String>,
    size: Option<i32>,
}

impl From<Pair<'_>> for Member {
    fn from(pair: Pair<'_>) -> Self {
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

impl Parseable<'_> for Member {
    const RULE: Rule = Rule::member;
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Const {
    name: String,
    value: Option<i32>,
    string: Option<String>,
}

impl From<Pair<'_>> for Const {
    fn from(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let value = inner.next().unwrap();
        match value.as_rule() {
            Rule::string => Const {
                name,
                string: Some(String::from(parse_string(value))),
                value: None,
            },
            Rule::number => Const {
                name,
                string: None,
                value: Some(parse_number(value)),
            },
            _ => unreachable!(),
        }
    }
}

impl Parseable<'_> for Const {
    const RULE: Rule = Rule::const_decl;
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Function {
    name: String,
    org: Option<i32>,
    typename: Option<String>,
    inline: bool,
}

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
}
