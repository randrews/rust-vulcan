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
            .op(Op::postfix(Rule::arglist))
            .op(Op::postfix(Rule::subscript))
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
    fn from_pair_located(pair: Pair) -> Located<Self> {
        let loc = pair.line_col();
        Located {
            location: loc.into(),
            ast: Self::from_pair(pair)
        }
    }
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
            Rule::const_decl => Self::Const(Const::from_pair(pair)),
            Rule::function_prototype => Self::Prototype(FunctionPrototype::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Global {
    const RULE: Rule = Rule::global;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let size = inner.next().map(Expr::from_pair);

        Global { name, size }
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
            .map(|p| String::from(p.as_str()))
            .collect();

        let body = Block::from_pair(inner.next().unwrap());

        Self { name, args, body }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for FunctionPrototype {
    const RULE: Rule = Rule::function_prototype;
    fn from_pair(pair: Pair<'_>) -> Self {
        let mut inner = pair.into_inner().peekable();
        let name = String::from(inner.next().unwrap().as_str());
        let args: Vec<_> = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|p| String::from(p.as_str()))
            .collect();

        Self { name, args }
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
            Rule::asm => Self::Asm(Asm::from_pair(pair)),
            _ => unreachable!(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Asm {
    const RULE: Rule = Rule::asm;
    fn from_pair(pair: Pair) -> Self {
        let mut args = Vec::new();
        let mut body = None;
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::expr => args.push(Expr::from_pair(p).into()),
                Rule::asm_body => body = Some(String::from(p.as_str().trim())),
                _ => unreachable!()
            }
        }
        Self { args, body: body.unwrap() }
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
        let lvalue = Expr::from_pair(pairs.next().unwrap()).into();
        let rvalue = Expr::from_pair(pairs.next().unwrap());
        Self { lvalue, rvalue }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for VarDecl {
    const RULE: Rule = Rule::var_decl;
    fn from_pair(pair: Pair) -> Self {
        let mut inner = pair.into_inner();
        let name = String::from(inner.next().unwrap().as_str());
        let mut size = None;
        let mut initial = None;

        for p in inner {
            match p.as_rule() {
                Rule::size => { size = Some(Expr::from_pair(p.first())) }
                Rule::expr => { initial = Some(Expr::from_pair(p)) }
                _ => unreachable!()
            }
        }

        Self { name, size, initial }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Block {
    const RULE: Rule = Rule::block;

    fn from_pair(pair: Pair) -> Self {
        Self(pair.into_inner().map(Statement::from_pair_located).collect())
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
            .map_primary(|term| match term.as_rule() {
                Rule::number => Expr::Number(term.into_number()),
                Rule::name => Expr::Name(String::from(term.as_str())),
                Rule::expr => Expr::from_pair(term),
                Rule::string => Self::String(term.into_quoted_string()),
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
                Rule::arglist => Expr::Call(
                    Call {
                        target: expr.into(),
                        args: suffix.into_inner().map(Expr::from_pair).collect(),
                    }
                ),
                Rule::subscript => {
                    Expr::Subscript(expr.into(), Expr::from_pair(suffix.first()).into())
                }
                _ => unreachable!(),
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

///////////////////////////////////////////////////////////////////////////////////////////

impl AstNode for Program {
    const RULE: Rule = Rule::program;
    fn from_pair(pair: Pair) -> Self {
        // Program captures EOI, to make sure that it's parsing the entire stream. We need to
        // ignore that though:
        Self(
            pair.into_inner()
                .filter(|p| p.as_rule() != Rule::EOI)
                .map(Declaration::from_pair_located)
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
                size: None,
            })
        );
        assert_eq!(
            Global::from_str("global foo[10];"),
            Ok(Global {
                name: "foo".into(),
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
                value: Some(Expr::Neg(7.into()).into()),
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
                args: vec!["a".into(), "b".into()],
                body: Block(vec![]),
            })
        );
    }

    #[test]
    fn parse_exprs() {
        use Expr::Infix;
        use Operator::*;

        // A very, very basic expression
        assert_eq!(Expr::from_str("23"), Ok(Expr::Number(23)));

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
        assert_eq!(Expr::from_str("-5"), Ok(Expr::Neg(5.into())));

        // Multiple prefixes
        assert_eq!(
            Expr::from_str("!-foo"),
            Ok(Expr::Not(Expr::Neg("foo".into()).into()))
        );

        // Simple suffix
        assert_eq!(
            Expr::from_str("foo[10]"),
            Ok(Expr::Subscript("foo".into(), 10.into()))
        );

        // Multi-suffix
        assert_eq!(
            Expr::from_str("foo[10][3]"),
            Ok(Expr::Subscript(
                Expr::Subscript("foo".into(), 10.into()).into(),
                3.into()
            ))
        );

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

        // Parens
        assert_eq!(
            Expr::from_str("(1 + 2) * 3"),
            Ok(Infix(Infix(1.into(), Add, 2.into()).into(), Mul, 3.into()))
        );

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
    fn parse_calls() {
        let blah = Expr::Call(Call { target: "blah".into(), args: vec![] });

        // Can Node parse a call?
        assert_eq!(Expr::from_str("blah()"), Ok(blah.clone()));

        // Can Statement parse a call?
        assert_eq!(Statement::from_str("blah();"), Ok(Statement::Expr(blah)));

        // Calls with args
        assert_eq!(
            Expr::from_str("blah(1, 2)"),
            Ok(Expr::Call(Call { target: "blah".into(), args: vec![1.into(), 2.into()] }))
        );

        //Calls with strings
        assert_eq!(
            Expr::from_str("blah(\"foo\", 2)"),
            Ok(Expr::Call(Call { target: "blah".into(), args: vec![Expr::String("foo".into()), 2.into()] }))
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
                rvalue: 7.into(),
            }))
        );

        assert_eq!(
            Assignment::from_str("foo[45] = 7"),
            Ok(Assignment {
                lvalue: Expr::Subscript("foo".into(), 45.into()).into(),
                rvalue: 7.into(),
            })
        );

        assert_eq!(
            Assignment::from_str("*foo = 12"),
            Ok(Assignment {
                lvalue: Expr::Deref("foo".into()).into(),
                rvalue: 12.into()
            })
        );
    }

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

        assert_eq!(
            VarDecl::from_str("var blah[7] = 35"),
            Ok(VarDecl {
                name: "blah".into(),
                size: Some(7.into()),
                initial: Some(35.into()),
            })
        );
    }

    #[test]
    fn parse_block() {
        let block = Block::from_str("{ foo(); bar(); }").unwrap();
        let statements: Vec<_> = block.0.into_iter().map(|s| s.ast).collect();
        assert_eq!(
            statements,
            vec![
                Statement::Expr(Expr::Call(Call { target: "foo".into(), args: vec![] })),
                Statement::Expr(Expr::Call(Call { target: "bar".into(), args: vec![] })),
            ]
        );
    }

    fn dislocate<T>(block: Vec<Located<T>>) -> Vec<T> {
        block.into_iter().map(|l| l.ast).collect()
    }

    fn dislocated_block(src: &str) -> Vec<Statement> {
        dislocate(Block::from_str(src).unwrap().0)
    }

    #[test]
    fn parse_conditional() {
        if let Ok(Statement::Conditional(Conditional { condition, body, alternative })) =
            Statement::from_str("if(cond) { foo(); }") {
            assert_eq!(condition, Expr::from_str("cond").unwrap());
            assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
            assert_eq!(alternative, None);
        } else {
            panic!()
        }

        if let Ok(Statement::Conditional(Conditional { condition, body, alternative })) =
            Statement::from_str("if(cond) { foo(); } else { bar(); }") {
            assert_eq!(condition, Expr::from_str("cond").unwrap());
            assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
            assert_eq!(alternative.unwrap().0[0].ast, Block::from_str("{ bar(); }").unwrap().0[0].ast);
        } else {
            panic!()
        }
    }

    #[test]
    fn parse_while_loops() {
        assert!(match Statement::from_str("while(cond) { foo(); }") {
            Ok(Statement::WhileLoop(WhileLoop { condition, body })) => {
                assert_eq!(condition, Expr::from_str("cond").unwrap());
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
                true
            }
            _ => false
        });
    }

    #[test]
    fn parse_repeat_loops() {
        assert!(match Statement::from_str("repeat(10) x { foo(x); }") {
            Ok(Statement::RepeatLoop(RepeatLoop { count, name, body })) => {
                assert_eq!(count, 10.into());
                assert_eq!(name, Some("x".into()));
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(x); }"));
                true
            },
            _ => false
        });

        assert!(match Statement::from_str("repeat(10) { foo(); }") {
            Ok(Statement::RepeatLoop(RepeatLoop { count, name, body })) => {
                assert_eq!(count, 10.into());
                assert_eq!(name, None);
                assert_eq!(dislocate(body.0), dislocated_block("{ foo(); }"));
                true
            },
            _ => false
        });
    }

    #[test]
    fn parse_asm() {
        assert_eq!(
            Asm::from_str("asm { push 34 }"),
            Ok(Asm { args: vec![], body: "push 34".into() })
        );

        assert_eq!(
            Asm::from_str("asm (&a) { swap 34\nstorew }"),
            Ok(Asm { args: vec![Expr::Address("a".into()).into()], body: "swap 34\nstorew".into() })
        );
    }

    #[test]
    fn parse_fn_prototype() {
        assert_eq!(
            FunctionPrototype::from_str("fn foo();"),
            Ok(FunctionPrototype {
                name: "foo".into(),
                args: vec![],
            })
        );
    }

    #[test]
    fn parse_program() {
        let prog = Program::from_str("global foo; const blah = 3;").unwrap();
        let decls: Vec<_> = dislocate(prog.0);
        assert_eq!(
            decls,
            vec![
                Declaration::from_str("global foo;").unwrap(),
                Declaration::from_str("const blah = 3;").unwrap(),
            ]
        )
    }
}
