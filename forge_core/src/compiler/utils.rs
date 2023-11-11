use std::collections::BTreeMap;

/// A variable, of various different types:
/// - Literals have static values and can just be pushed to the stack
/// - IndirectLabels contain the label pointing to the value (think `foo: .db 0`)
/// - Direct labels are the value themselves (think `call foo`)
/// - Locals contain an index into the local frame
#[derive(Clone, PartialEq, Debug)]
pub enum Variable {
    Literal(i32),
    IndirectLabel(String),
    DirectLabel(String),
    Local(usize),
}

/// Maps from names to the variables they represent
pub type Scope = BTreeMap<String, Variable>;

/// So we don't get confused between string-strings and assembly-label strings
pub type Label = String;

/// Look up a name first in the local scope, and failing that in the global scope.
pub fn lookup<'a>(name: &str, global_scope: &'a Scope, local_scope: &'a Scope) -> Option<&'a Variable> {
    if let Some(var) = local_scope.get(name) {
        Some(var)
    } else if let Some(var) = global_scope.get(name) {
        Some(var)
    } else {
        None
    }
}
