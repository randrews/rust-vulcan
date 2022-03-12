use vcore::opcodes::Opcode;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Directive { Db, Equ, Org }

impl From<&str> for Directive {
    fn from(value: &str) -> Self {
        use Directive::*;
        match value {
            ".db" => Db,
            ".equ" => Equ,
            ".org" => Org,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct VASMLine<'a> {
    label: Option<&'a str>,
    opcode: Option<Opcode>,
    directive: Option<Directive>
}

impl<'a> VASMLine<'a> {
    pub fn blank() -> VASMLine<'a> {
        VASMLine {
            label: None,
            opcode: None,
            directive: None
        }
    }

    pub fn for_opcode(opcode: Opcode) -> VASMLine<'a> {
        Self::blank().with_opcode(opcode)
    }

    pub fn with_opcode(self, opcode: Opcode) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            opcode: Some(opcode),
            directive: self.directive
        }
    }

    pub fn with_directive(self, directive: Directive) -> VASMLine<'a> {
        VASMLine {
            label: self.label,
            opcode: self.opcode,
            directive: Some(directive)
        }
    }

    pub fn with_label(self, label: &'a str) -> Self {
        VASMLine {
            label: Some(label),
            opcode: self.opcode,
            directive: self.directive
        }
    }
}
