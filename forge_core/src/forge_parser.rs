use pest::Parser;

mod inner {
    #[derive(Parser)]
    #[grammar = "forge.pest"]
    pub struct ForgeParser;
}

use inner::*;
use pest::error::{Error, LineColLocation};
use std::str::FromStr;

type Pair<'a> = pest::iterators::Pair<'a, Rule>;

fn parse_i32(pair: Pair) -> i32 {
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
fn create_string(pair: Pair) -> String {
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

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Node {
    Function {
        name: String,
        org: Option<i32>,
        typename: Option<String>,
        inline: bool,
    },
    Global {
        name: String,
        typename: Option<String>,
    },
    Struct {
        name: String,
        members: Vec<Member>,
    },
    Const {
        name: String,
        value: Option<i32>,
        string: Option<String>,
    },
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Member {
    name: String,
    typename: Option<String>,
    size: Option<i32>,
}

pub fn string_to_ast(string: &str) -> Result<Vec<Node>, ParseError> {
    let pairs = ForgeParser::parse(Rule::program, string)
        .map_err(ParseError::from)?
        .next()
        .unwrap()
        .into_inner();

    let mut decls = Vec::new();
    for pair in pairs {
        let r = pair.as_rule();
        match r {
            Rule::function => {}
            Rule::global => decls.push(parse_global(pair)),
            Rule::struct_decl => decls.push(parse_struct(pair)),
            Rule::const_decl => decls.push(parse_const(pair)),
            Rule::EOI => {} // Ignore EOI; we have to capture it but it doesn't do anything
            _ => unreachable!(),
        }
    }

    Ok(decls)
}

fn parse_global(global: Pair) -> Node {
    let mut inner = global.into_inner();
    let name = String::from(inner.next().unwrap().as_str());
    let typename = inner
        .next()
        .map(|p| String::from(p.into_inner().next().unwrap().as_str()));
    Node::Global { name, typename }
}

fn parse_const(pair: Pair) -> Node {
    let mut inner = pair.into_inner();
    let name = String::from(inner.next().unwrap().as_str());
    let value = inner.next().unwrap();
    match value.as_rule() {
        Rule::string => Node::Const {
            name,
            string: Some(String::from(create_string(value))),
            value: None,
        },
        Rule::number => Node::Const {
            name,
            string: None,
            value: Some(parse_i32(value)),
        },
        _ => unreachable!(),
    }
}

fn parse_member(pair: Pair) -> Member {
    let mut inner = pair.into_inner();
    let mut member = Member {
        name: String::from(inner.next().unwrap().as_str()),
        typename: None,
        size: None,
    };
    for child in inner {
        match child.as_rule() {
            Rule::number => member.size = Some(parse_i32(child)),
            Rule::typename => {
                member.typename = Some(String::from(child.into_inner().next().unwrap().as_str()))
            }
            _ => unreachable!(),
        }
    }
    member
}

fn parse_struct(pair: Pair) -> Node {
    let mut inner = pair.into_inner();
    let name = String::from(inner.next().unwrap().as_str());
    let mut member_pairs = inner.next().unwrap().into_inner();
    let members: Vec<_> = member_pairs.map(|member| parse_member(member)).collect();
    Node::Struct { name, members }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_globals() {
        let ast = string_to_ast("global foo;");
        assert_eq!(
            ast,
            Ok(vec![Node::Global {
                name: "foo".into(),
                typename: None
            }])
        );

        let ast = string_to_ast("global bar:Sometype;");
        assert_eq!(
            ast,
            Ok(vec![Node::Global {
                name: "bar".into(),
                typename: Some("Sometype".into())
            }])
        );
    }

    #[test]
    fn parse_consts() {
        assert_eq!(
            string_to_ast("const a = 123;"),
            Ok(vec![Node::Const {
                name: "a".into(),
                value: Some(123),
                string: None
            }])
        );
        assert_eq!(
            string_to_ast("const a = 0xaa;"),
            Ok(vec![Node::Const {
                name: "a".into(),
                value: Some(0xaa),
                string: None
            }])
        );
        assert_eq!(
            string_to_ast("const a = -7;"),
            Ok(vec![Node::Const {
                name: "a".into(),
                value: Some(-7),
                string: None
            }])
        );
        assert_eq!(
            string_to_ast("const a = \"foo bar\";"),
            Ok(vec![Node::Const {
                name: "a".into(),
                value: None,
                string: Some("foo bar".into())
            }])
        )
    }

    #[test]
    fn parse_structs() {
        assert_eq!(
            string_to_ast("struct Point { x, y }"),
            Ok(vec![Node::Struct {
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
            }])
        );

        assert_eq!(
            string_to_ast("struct Foo { bar[100] }"),
            Ok(vec![Node::Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: None,
                    size: Some(100)
                },]
            }])
        );

        assert_eq!(
            string_to_ast("struct Foo { bar:Thing[100] }"),
            Ok(vec![Node::Struct {
                name: "Foo".into(),
                members: vec![Member {
                    name: "bar".into(),
                    typename: Some("Thing".into()),
                    size: Some(100)
                },]
            }])
        );
    }
}
