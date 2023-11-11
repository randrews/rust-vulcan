use crate::ast::Location;
use crate::compiler::compile_error::CompileError;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::state::State;

pub(crate) trait Compilable {
    // Every AST node we visit can see the global compiler state (to make unique
    // symbols and reach for global names) as well as optionally the current
    // function (for AST nodes within functions). Additionally se send a Location:
    // This is the closest ancestor's location, in case we need to emit a compile
    // error
    fn process(
        self,
        state: &mut State,
        function: Option<&mut CompiledFn>,
        location: Location,
    ) -> Result<(), CompileError>;
}