use pest::pratt_parser::PrattParser;
use pest::Parser;

use crate::ast::*;
pub use pairs_ext::*;
pub use parse_error::ParseError;

mod parse_error;
mod pairs_ext;
mod ast_nodes;

#[cfg(test)]
mod test_utils;

#[derive(Parser)]
#[grammar = "parser/forge.pest"]
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

/// Make some Pest types a little more ergonomic to refer to, especially outside this file:
pub type PestRule = Rule;
pub(crate) type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub(crate) type Pairs<'i, R = Rule> = pest::iterators::Pairs<'i, R>;

/// A trait that represents something that can be parsed into an AST node: impl this to turn
/// a pair into your node, by from_pair-ing child nodes.
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

/// A companion trait to AstNode: Parseable things can be parsed from strings, and Parseable
/// AstNodes have a default way of doing that (using the ForgeParser to make a pair and then
/// from_pair-ing it
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

/// Try and parse a program into an AST node. A convenience method (since Program is both
/// Parseable and an AstNode) but also the outside entry point into this whole module.
pub fn parse(src: &str) -> Result<Program, ParseError> {
    Program::from_str(src)
}

///////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
impl Expr {
    pub(crate) fn parse(src: &str) -> Result<Self, ParseError> {
        Self::from_str(src)
    }
}
