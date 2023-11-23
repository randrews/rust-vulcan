use std::fmt::Display;

#[derive(Default, Clone, PartialEq, Debug)]
pub struct Text(pub Vec<String>);

impl Text {
    /// Emit a string (ideally one instruction, but whatever) to the function body.
    /// This doesn't actually emit anything to output, the body will eventually be emitted
    /// in a final pass by the compiler once all functions are compiled.
    pub(crate) fn emit(&mut self, opcode: &str) {
        self.0.push(String::from(opcode))
    }

    /// A shorthand method to emit something with a `Display` arg, because emitting a
    /// single instruction with a variable (numeric or label) arg is very common.
    pub(crate) fn emit_arg<T: Display>(&mut self, opcode: &str, arg: T) {
        self.0.push(format!("{} {}", opcode, arg))
    }

    /// Append another `Text` to this one
    pub(crate) fn append(&mut self, other: &mut Text) {
        self.0.append(&mut other.0)
    }
}