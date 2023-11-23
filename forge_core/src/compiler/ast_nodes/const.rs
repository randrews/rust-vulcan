use crate::ast::{Const, Expr, Location, Operator};
use crate::compiler::compilable::Compilable;
use crate::compiler::compiled_fn::CompiledFn;
use crate::compiler::CompileError;
use crate::compiler::state::State;
use crate::compiler::utils::{Scope, Variable};

impl Compilable for Const {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, _loc: Location) -> Result<(), CompileError> {
        let var = if self.string.is_some() {
            // If it's a string, add it to the string table
            Variable::DirectLabel(state.add_string(&self.string.unwrap()))
        } else if let Some(expr) = self.value {
            // Otherwise eval_const it
            Variable::Literal(eval_const(expr, &state.global_scope)?)
        } else {
            unreachable!()
        };
        // Add it to the global namespace
        state.add_global(&self.name, |_| var.clone())
    }
}

/// Evaluate a node in a static context, for const definitions and array sizes, that sort of thing.
pub fn eval_const(expr: Expr, scope: &Scope) -> Result<i32, CompileError> {
    match expr {
        Expr::Number(n) => Ok(n), // That was easy
        Expr::Name(n) => {
            if let Some(Variable::Literal(val)) = scope.get(&n) {
                Ok(*val)
            } else {
                Err(CompileError(0, 0, format!("Unknown const {}", n)))
            }
        }
        Expr::Neg(e) => {
            let val = eval_const(*e.0, scope)?;
            Ok(-val)
        }
        Expr::Not(e) => {
            let val = eval_const(*e.0, scope)?;
            Ok(if val != 0 { 0 } else { 1 })
        }
        // A note about Expr::String here: 'const foo="banana"' won't hit this point; it'll be
        // (currently) parsed as a special case of const. The only things that will hit this are
        // const expressions that include strings, like '"foo"[2]' or something. Those can't be
        // (generally) calculated at compile time, so, they're an error.
        Expr::Address(_) | Expr::Deref(_) | Expr::String(_) | Expr::Static(_) => Err(CompileError(
            0,
            0,
            String::from("Addresses are not known at compile time"),
        )),
        Expr::Call(_) | Expr::Subscript(_, _) | Expr::New(_) => Err(CompileError(
            0,
            0,
            String::from("Constants must be statically defined"),
        )),
        Expr::Infix(lhs, op, rhs) => {
            let lhs = eval_const(*lhs.0, scope)?;
            let rhs = eval_const(*rhs.0, scope)?;
            match op {
                Operator::Add => Ok(lhs + rhs),
                Operator::Sub => Ok(lhs - rhs),
                Operator::Mul => Ok(lhs * rhs),
                Operator::Div => Ok(lhs / rhs),
                Operator::Mod => Ok(lhs % rhs),
                Operator::And => Ok(to_flag(lhs != 0 && rhs != 0)),
                Operator::Or => Ok(to_flag(lhs != 0 || rhs != 0)),
                Operator::BitAnd => Ok(lhs & rhs),
                Operator::BitOr => Ok(lhs | rhs),
                Operator::Xor => Ok(lhs ^ rhs),
                Operator::Lt => Ok(to_flag(lhs < rhs)),
                Operator::Le => Ok(to_flag(lhs <= rhs)),
                Operator::Gt => Ok(to_flag(lhs > rhs)),
                Operator::Ge => Ok(to_flag(lhs >= rhs)),
                Operator::Eq => Ok(to_flag(lhs == rhs)),
                Operator::Ne => Ok(to_flag(lhs != rhs)),
                Operator::Lshift => Ok(lhs << rhs),
                Operator::Rshift => Ok(lhs >> rhs),
            }
        }
    }
}

/// Vulcan sees boolean flags as 1 or 0, Rust has an actual bool type. Bridge the gap:
fn to_flag(val: bool) -> i32 {
    if val {
        1
    } else {
        0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_eval_const() {
        let empty_scope = Scope::new();
        let to_expr = |s| Expr::parse(s).unwrap();

        // Basic arithmetic
        assert_eq!(eval_const(to_expr("2 * 3 + 4"), &empty_scope), Ok(10));
        assert_eq!(eval_const(to_expr("1 + -2"), &empty_scope), Ok(-1));
        assert_eq!(eval_const(to_expr("1 << 3"), &empty_scope), Ok(8));

        // Names
        let scope: Scope = [
            ("foo".into(), Variable::Literal(10)),
            ("bar".into(), Variable::Literal(5)),
        ]
            .into();
        assert_eq!(eval_const(to_expr("foo + 5"), &scope), Ok(15));
        assert_eq!(eval_const(to_expr("bar * foo"), &scope), Ok(50));

        // Error
        assert_eq!(
            eval_const(to_expr("nope"), &scope),
            Err(CompileError(0, 0, String::from("Unknown const nope")))
        );
    }

    #[test]
    fn test_const_decl() {
        let mut state = State::default();
        parse("const foo = 17 + 3;")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("foo".into(), Variable::Literal(20))].into()
        )
    }

    #[test]
    fn test_string_const() {
        let mut state = State::default();
        parse("const foo = \"bar\";")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("foo".into(), Variable::DirectLabel("_forge_gensym_1".into()))].into()
        )
    }
}