use vcore::opcodes::Opcode;

use crate::parse_error::ParseError;
use crate::vasm_line::{Directive, VASMLine};

use pest::{Parser};
use std::convert::TryFrom;

#[derive(Parser)]
#[grammar = "vasm.pest"]
struct VASMParser;

fn parse_vasm_line(line: &str) -> Result<VASMLine, ParseError<'_>> {
    let mut pairs = VASMParser::parse(Rule::line, line)
        .map_err(|_| ParseError("Failed to parse line"))?
        .next()
        .unwrap()
        .into_inner()
        .peekable();

    let mut vasm_line = VASMLine::blank();

    if let Some(label_group) = pairs.next_if(|pair| pair.as_rule() == Rule::label_group) {
        let label = label_group.into_inner().next().unwrap();
        vasm_line = vasm_line.with_label(label.as_str())
    }

    if let Some(instruction_group) = pairs.next_if(|pair| pair.as_rule() == Rule::instruction_group) {
        let instruction_field = instruction_group.into_inner().next().unwrap();
        vasm_line = match instruction_field.as_rule() {
            Rule::opcode => vasm_line.with_opcode(Opcode::try_from(instruction_field.as_str())?),
            Rule::directive => vasm_line.with_directive(Directive::from(instruction_field.as_str())),
            _ => unreachable!()
        }
        // something something argument here
    }

    Ok(vasm_line)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse_vasm_line("add"), Ok(VASMLine::for_opcode(Opcode::Add)));
        assert_eq!(parse_vasm_line("sub"), Ok(VASMLine::for_opcode(Opcode::Sub)));
        assert_eq!(parse_vasm_line("blah"), Err(ParseError("Invalid opcode")));
    }

    #[test]
    fn test_parse_labels() {
        assert_eq!(parse_vasm_line("foo: add"), Ok(VASMLine::for_opcode(Opcode::Add).with_label("foo")));
        assert_eq!(parse_vasm_line("bar:"), Ok(VASMLine::blank().with_label("bar")));
    }

    #[test]
    fn test_parse_directives() {
        assert_eq!(parse_vasm_line("foo: .equ"), Ok(VASMLine::blank().with_label("foo").with_directive(Directive::Equ)));
        assert_eq!(parse_vasm_line(".db"), Ok(VASMLine::blank().with_directive(Directive::Db)));
    }
}
