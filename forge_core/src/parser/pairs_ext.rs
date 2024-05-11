use std::iter::Peekable;
use std::str::FromStr;
use crate::parser::{Pair, Pairs, PestRule};

/// Some handy utility methods to extend Pair with
pub trait PairExt {
    /// Go down one level in the AST, return the first node. Assumes the first node exists
    fn first(self) -> Self;

    /// Just like `first` but panics if there's more than one child node.
    //fn only(self) -> Self;

    /// This should really be an AstNode or something, but it's used so often: turn a Pair into
    /// an i32 by parsing the Forge number format (0xwhatever, 0bwhatever, etc)
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

    fn into_number(self) -> i32 {
        let first = self.into_inner().next().unwrap();
        match first.as_rule() {
            PestRule::dec_number | PestRule::dec_zero => i32::from_str(first.as_str()).unwrap(),
            PestRule::hex_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 16).unwrap(),
            PestRule::bin_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 2).unwrap(),
            PestRule::oct_number => i32::from_str_radix(first.as_str().get(2..).unwrap(), 8).unwrap(),
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

/// Some handy utility methods to extend Pairs with
pub trait PairsExt {
    /// Peek the next sibling but only if it matches a given rule: used for things
    /// with optional modifiers following
    fn next_if_rule(&mut self, rule: PestRule) -> Option<Pair>;
}

impl PairsExt for Peekable<Pairs<'_>> {
    fn next_if_rule(&mut self, rule: PestRule) -> Option<Pair> {
        self.next_if(|p| p.as_rule() == rule)
    }
}
