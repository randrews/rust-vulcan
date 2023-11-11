use crate::ast::{FunctionPrototype, Location};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;

impl Compilable for FunctionPrototype {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        state.declare_function(self.name.as_str(), self.args)
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;
    use crate::compiler::utils::Variable;

    #[test]
    fn test_function_prototypes() {
        let mut state = state_for("fn blah(a, b);");
        assert_eq!(state.functions.get("blah"), None);
        assert_eq!(state.prototypes.remove("blah"), Some(Variable::DirectLabel(String::from("_forge_gensym_1"))));
    }

    #[test]
    fn test_functions_with_prototypes() {
        let mut state = state_for("fn blah(a, b); const foo = 3; fn blah(a, b) { return 7; }");
        assert_eq!(state.functions.remove("blah").unwrap().label, String::from("_forge_gensym_1"));
        assert_eq!(state.prototypes.remove("blah"), None);
    }
}