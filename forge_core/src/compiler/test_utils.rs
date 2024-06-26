use crate::compiler::compilable::Compilable;
use crate::compiler::CompileError;
use crate::compiler::state::State;
use crate::parser::parse;

pub(crate) fn state_for(src: &str) -> State {
    let mut state = State::default();
    parse(src)
        .unwrap()
        .process(&mut state, None, (0, 0).into())
        .expect("Failed to compile");
    state
}

pub(crate) fn test_body(state: State) -> String {
    state.functions.get("test").unwrap().body.0.join("\n")
}

pub(crate) fn test_err(src: &str) -> Option<CompileError> {
    let mut state = State::default();
    parse(src).unwrap().process(&mut state, None, (0, 0).into()).err()
}

pub(crate) fn test_preamble(state: State) -> String {
    state.functions.get("test").unwrap().preamble.0.join("\n")
}

pub(crate) fn test_outro(state: State) -> String {
    state.functions.get("test").unwrap().outro.0.join("\n")
}
