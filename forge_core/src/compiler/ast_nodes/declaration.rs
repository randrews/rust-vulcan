use crate::ast::{Declaration, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for Declaration {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        match self {
            Declaration::Function(f) => f.process(state, None, loc),
            Declaration::Global(g) => g.process(state, None, loc),
            Declaration::Const(c) => c.process(state, None, loc),
            Declaration::Prototype(p) => p.process(state, None, loc),
        }
    }
}
