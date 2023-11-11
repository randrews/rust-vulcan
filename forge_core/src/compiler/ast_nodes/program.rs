use crate::ast::{Location, Program};
use crate::compiler::compilable::Compilable;
use crate::compiler::compile_error::CompileError;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::state::State;

impl Compilable for Program {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        for decl in self.0 {
            decl.ast.process(state, None, decl.location)?
        }
        Ok(())
    }
}