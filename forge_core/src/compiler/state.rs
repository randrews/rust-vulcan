use std::collections::BTreeMap;
use crate::ast::Location;
use crate::compiler::compile_error::CompileError;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::text::Text;
use crate::compiler::utils::{Label, Scope, Variable};

/// The compiler state:
#[derive(Clone, PartialEq, Debug, Default)]
pub(crate) struct State {
    /// Used by gensym to generate unique symbols
    pub gensym_index: usize,
    /// The globally-defined names
    pub global_scope: Scope,
    /// The functions
    pub functions: BTreeMap<String, CompiledFn>,
    /// The string table
    pub strings: Vec<(Label, String)>,
    /// The statically-allocated buffers (size in bytes)
    pub buffers: Vec<(Label, usize)>,
    /// The functions that have been prototyped but not yet defined
    pub prototypes: Scope,
    /// The initialization code for globals
    pub init: Text,
}

impl State {
    /// Generate a guaranteed-unique symbolic name
    pub(crate) fn gensym(&mut self) -> Label {
        self.gensym_index += 1;
        format!("_forge_gensym_{}", self.gensym_index)
    }

    /// Return whether a name exists in the global scope already
    fn defined(&self, name: &str) -> bool {
        self.global_scope.contains_key(name)
    }

    /// Add a symbol to the global namespace, catching name collisions
    pub(crate) fn add_global<F: Fn(&mut State) -> Variable>(
        &mut self,
        name: &str,
        val: F,
    ) -> Result<(), CompileError> {
        if self.defined(name) {
            Err(CompileError(0, 0, format!("name {} already defined", name)))
        } else {
            let val = val(self);
            self.global_scope.insert(name.into(), val);
            Ok(())
        }
    }

    pub(crate) fn add_string(&mut self, string: &str) -> Label {
        let sym = self.gensym();
        self.strings.push((sym.clone(), string.into()));
        sym
    }

    pub(crate) fn add_buffer(&mut self, size: usize) -> Label {
        let sym = self.gensym();
        self.buffers.push((sym.clone(), size));
        sym
    }

    pub(crate) fn declare_function(&mut self, name: &str, _args: Vec<String>) -> Result<(), CompileError> {
        // If it's not already prototyped, gensym a label and put it in the list. If it is, just
        // ignore this (we don't check arity so it's not like the arglist being different matters)
        // todo: check arity, store it here
        if !self.prototypes.contains_key(name) {
            let label = self.gensym();
            self.add_global(name, |_| Variable::DirectLabel(label.clone()))?;
            self.prototypes.insert(name.into(), Variable::DirectLabel(label));
        }
        Ok(())
    }

    pub(crate) fn find_or_declare_function(&mut self, name: &str, _loc: Location) -> Result<String, CompileError> {
        // If it's in here, remove it and return the label:
        // (this will only ever match this way; only thing that puts stuff in prototypes adds directlabels)
        if let Some(Variable::DirectLabel(label)) = self.prototypes.remove(name) {
            Ok(label)
        } else {
            // Otherwise, make a new label, add it to globals, and return it
            let label = self.gensym();
            self.add_global(name, |_| Variable::DirectLabel(label.clone()))?;
            Ok(label)
        }
    }
}
