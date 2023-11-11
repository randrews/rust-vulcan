use crate::ast::{Assignment, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Assignment {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let Assignment { lvalue, rvalue } = self;
        let sig = sig.expect("Assignment outside function");
        // First the value, then the address we'll storew it to
        rvalue.process(state, Some(sig), loc)?;
        lvalue.process(state, Some(sig), loc)?;
        sig.emit("storew");
        Ok(())
    }
}
