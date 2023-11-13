use crate::ast::{BoxExpr, Expr, Location, Operator};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::{CompileError};
use crate::compiler::ast_nodes::r#const::eval_const;
use crate::compiler::state::State;
use crate::compiler::utils::{lookup, Variable};

/// Evaluate an expression in the context of a local scope. The runtime brother to eval_const.
/// This recursively evaluates a Node and leaves its value on the stack.
impl Compilable for Expr {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let mut sig = sig.expect("Non-const expression outside a function");
        let global_scope = &state.global_scope;

        // First, a sanity check: try and eval_const this. If it's something incredibly basic
        // that just becomes an i32, then we don't need to do anything else:
        if let Ok(val) = eval_const(self.clone(), &state.global_scope) {
            sig.emit_arg("push", val);
            return Ok(());
        }

        // Okay, looks like we need something that's in scope. Let's recurse:
        match self {
            Expr::Number(n) => {
                // Numbers are just pushed as literals
                sig.body.push(format!("push {}", n));
                Ok(())
            }
            Expr::Name(name) => {
                match lookup(&name, global_scope, &sig.local_scope) {
                    // Names are treated differently depending on what they are
                    Some(Variable::Literal(val)) => {
                        // Names of constants are just that number
                        sig.emit_arg("push", *val);
                        Ok(())
                    }
                    Some(Variable::IndirectLabel(label)) => {
                        // Names pointing at labels are loaded (rvalue; for lvalues they aren't)
                        // Indirect labels are the address of where the value is stored (a var, .db)
                        sig.emit_arg("loadw", label.clone());
                        Ok(())
                    }
                    Some(Variable::DirectLabel(label)) => {
                        // Direct labels are like functions, the label itself is the value, so just
                        // push it:
                        sig.emit_arg("push", label.clone());
                        Ok(())
                    }
                    Some(Variable::Local(offset)) => {
                        // Names of locals are added from the frame pointer
                        let offset = *offset;

                        sig.emit("peekr"); // Frame ptr is top of rstack
                        if offset > 0 {
                            sig.emit_arg("add", offset);
                        }
                        sig.emit("loadw");
                        Ok(())
                    }
                    None => Err(CompileError(0, 0, format!("Unknown name {}", name))),
                }
            }
            Expr::Neg(e) => {
                (*e.0).process(state, Some(sig), loc)?;
                // To arithmetically negate something, invert and increment (2s complement)
                sig.emit("xor -1");
                sig.emit("add 1");
                Ok(())
            }
            Expr::Not(e) => {
                (*e.0).process(state, Some(sig), loc)?;
                sig.emit("not");
                Ok(())
            }
            // Handling addresses is very easy because processing an lvalue leaves the address on the stack
            Expr::Address(lvalue) => lvalue.process(state, Some(sig), loc),
            Expr::Deref(BoxExpr(e)) => {
                (*e).process(state, Some(sig), loc)?;
                sig.emit("loadw");
                Ok(())
            }
            Expr::String(string) => {
                let label = state.add_string(&string);
                sig.emit_arg("push", label);
                Ok(())
            }
            Expr::Call(call) => call.process(state, Some(sig), loc),
            Expr::New(_size) => todo!("new not yet supported"),
            Expr::Subscript(_, _) => todo!("Structs and arrays are not yet supported"),
            Expr::Infix(lhs, op, rhs) => {
                // Recurse on expressions, handling operators
                (*lhs.0).process(state, Some(&mut sig), loc)?;
                (*rhs.0).process(state, Some(&mut sig), loc)?;
                match op {
                    // Basic math
                    Operator::Add => sig.emit("add"),
                    Operator::Sub => sig.emit("sub"),
                    Operator::Mul => sig.emit("mul"),
                    Operator::Div => sig.emit("div"),
                    Operator::Mod => sig.emit("mod"),
                    Operator::And => {
                        // Vulcan "and" is bitwise, so we need to flag-ify both args to make it logical
                        sig.emit("gt 0");
                        sig.emit("swap");
                        sig.emit("gt 0");
                        sig.emit("and");
                    }
                    Operator::Or => {
                        // Same as and, flag-ify both args
                        sig.emit("gt 0");
                        sig.emit("swap");
                        sig.emit("gt 0");
                        sig.emit("or");
                    }
                    Operator::BitAnd => sig.emit("and"),
                    Operator::BitOr => sig.emit("or"),
                    Operator::Xor => sig.emit("xor"),
                    Operator::Lt => sig.emit("alt"),
                    Operator::Le => {
                        // LE and GE are the inverses of GT and LT (arithmetic versions)
                        sig.emit("agt");
                        sig.emit("not");
                    }
                    Operator::Gt => sig.emit("agt"),
                    Operator::Ge => {
                        sig.emit("alt");
                        sig.emit("not");
                    }
                    Operator::Eq => {
                        sig.emit("xor");
                        sig.emit("not");
                    }
                    Operator::Ne => sig.emit("xor"),
                    Operator::Lshift => sig.emit("lshift"),
                    Operator::Rshift => sig.emit("arshift"),
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn test_literal_strings() {
        let state = state_for("const s1 = \"foo\"; fn test() { var x; x = \"bar\"; var y = \"norp\"; }");
        assert_eq!(
            state.strings,
            vec![
                ("_forge_gensym_1".into(), "foo".into()),
                ("_forge_gensym_4".into(), "bar".into()), // gensym 2 and 3 are the entry and outro of blah()
                ("_forge_gensym_5".into(), "norp".into()),
            ]
        );
        assert_eq!(
            test_body(state),
            vec![
                "push _forge_gensym_4",
                "peekr",
                "storew", // the assignment for x
                "push _forge_gensym_5",
                "peekr",
                "add 3", // the address of y (frame + 3) and put gensym_4 in it
                "storew",
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_addresses() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; var y = &x; }")),
            vec![
                "push 3",
                "peekr",
                "storew",      // the assignment for x
                "peekr", // The addr of x
                "peekr",
                "add 3", // the address of y (frame + 3) and put the addr of x (frame) in it
                "storew",
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_derefs() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; *1000 = *x; }")),
            vec![
                "push 3",
                "peekr",
                "storew",      // the assignment for x
                "peekr", // Now we're compiling *x, so load x's value, which is 3
                "loadw",
                "loadw", // Then load the value at 3
                "push 1000", // Push the addr 1000, for the lvalue
                "storew", // Store whatever's at 3 to 1000
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_string_exprs() {
        assert_eq!(
            test_body(state_for("const foo = \"foo\"; fn test() { var x = \"bar\" + 3; }")),
            vec![
                "push _forge_gensym_4", // 1 is the label in the string table for "foo", 2 for "blah," 3 for blah's outro
                "push 3", // so 3 is the string "bar"
                "add", // Add 3 to that address
                "peekr", // Store it in the first var
                "storew",
            ]
                .join("\n")
        )
    }
}