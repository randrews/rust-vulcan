use crate::ast::{BoxExpr, Expr, Location, Lvalue};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;
use crate::compiler::utils::{lookup, Variable};

/// Evaluate an lvalue and leave its address on the stack (ready to be consumed by storew)
impl Compilable for Lvalue {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let global_scope = &state.global_scope;
        let sig = sig.expect("lvalue outside a function");

        match *(self.0.0) {
            // A bunch of different things that aren't allowed
            Expr::Number(_) |
            Expr::Neg(_) |
            Expr::Not(_) |
            Expr::Address(_) |
            Expr::Call(_) |
            Expr::New(_) |
            Expr::Static(_) |
            Expr::Infix(_, _, _) |
            Expr::String(_) => {
                Err(CompileError(0, 0, String::from("Not a valid lvalue")))
            }

            // Names we look up and leave the address on the stack:
            Expr::Name(name) => {
                if let Some(var) = lookup(&name, global_scope, &sig.local_scope) {
                    match var {
                        Variable::Literal(_) | Variable::DirectLabel(_) => {
                            // Direct labels are (probably) functions, the important part is the
                            // label itself, which we can't alter, so, error:
                            Err(CompileError(0, 0, format!("Invalid lvalue {}", name)))
                        }
                        Variable::IndirectLabel(label) => {
                            // Indirect labels are variables, the label is where the data is stored,
                            // so we push that label so we can store stuff there
                            let label = label.clone();
                            sig.emit_arg("push", label);
                            Ok(())
                        }
                        Variable::Local(offset) => {
                            let offset = *offset;
                            sig.emit("peekr");
                            if offset > 0 {
                                sig.emit_arg("add", offset);
                            }
                            Ok(())
                        }
                    }
                } else {
                    Err(CompileError(0, 0, format!("Unknown name {}", name)))
                }
            }
            // Derefs are just evaluating the expr and leaving its value (an address) on the stack
            Expr::Deref(BoxExpr(expr)) => {
                (*expr).process(state, Some(sig), loc)
            }
            Expr::Subscript(array, index) => {
                array.0.process(state, Some(sig), loc)?;
                index.0.process(state, Some(sig), loc)?;
                sig.emit_arg("mul", 3); // Indices are in words, convert to byte offset
                sig.emit("add"); // Add offset
                Ok(())
            }
        }
    }
}
