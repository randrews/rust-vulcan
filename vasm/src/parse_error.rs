use vcore::opcodes::InvalidOpcode;

#[derive(Debug, PartialEq)]
pub struct ParseError<'a>(pub &'a str);

impl<'a> From<InvalidOpcode> for ParseError<'a> {
    fn from(_: InvalidOpcode) -> Self {
        Self("Invalid opcode")
    }
}
