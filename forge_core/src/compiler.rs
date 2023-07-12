use crate::ast::*;
use crate::forge_parser::parse;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};

#[derive(Eq, Clone, PartialEq, Debug)]
pub struct CompileError(usize, usize, String);

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.0, self.1, self.2)
    }
}

/// Maps from names to numeric values: for consts, these are the actual values;
/// for globals, the labels for where they're stored
pub type ConstScope = BTreeMap<String, i32>;
pub type GlobalScope = BTreeMap<String, String>;

#[derive(Clone, PartialEq, Debug, Default)]
struct State {
    const_scope: ConstScope,
    gensym_index: usize,
    global_scope: GlobalScope,
}

impl State {
    /// Generate a guaranteed-unique symbolic name
    fn gensym(&mut self) -> String {
        self.gensym_index += 1;
        format!("_gensym_{}", self.gensym_index)
    }

    /// Return whether a name exists in the global scope already
    fn defined(&self, name: &str) -> bool {
        self.const_scope.contains_key(name) || self.global_scope.contains_key(name)
    }
}

trait Compilable {
    fn process(self, state: &mut State) -> Result<(), CompileError>;
}

///////////////////////////////////////////////////////////

impl Compilable for Program {
    fn process(self, state: &mut State) -> Result<(), CompileError> {
        for decl in self.0 {
            decl.process(state)?
        }
        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Declaration {
    fn process(self, state: &mut State) -> Result<(), CompileError> {
        match self {
            Declaration::Function(_) => todo!(),
            Declaration::Global(g) => g.process(state),
            Declaration::Struct(_) => todo!(),
            Declaration::Const(c) => c.process(state),
        }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Global {
    fn process(self, state: &mut State) -> Result<(), CompileError> {
        if self.typename.is_some() || self.size.is_some() {
            todo!("Structs and arrays are not yet supported")
        }
        if state.defined(&self.name) {
            Err(CompileError(
                0,
                0,
                format!("name {} already defined", self.name),
            ))
        } else {
            let label = state.gensym();
            state.global_scope.insert(self.name, label);
            Ok(())
        }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Const {
    fn process(self, state: &mut State) -> Result<(), CompileError> {
        if let Some(_) = self.string {
            todo!("Strings are not yet supported")
        }

        if let Some(expr) = self.value {
            let val = eval_const(expr, &state.const_scope)?;
            if state.defined(self.name.as_str()) {
                Err(CompileError(
                    0,
                    0,
                    format!("name {} already defined", self.name),
                ))
            } else {
                state.const_scope.insert(self.name, val);
                Ok(())
            }
        } else {
            unreachable!()
        }
    }
}

///////////////////////////////////////////////////////////

fn to_flag(val: bool) -> i32 {
    if val {
        1
    } else {
        0
    }
}

/// Evaluate a node in a static context, for const definitions and array sizes, that sort of thing.
pub fn eval_const(expr: Node, scope: &ConstScope) -> Result<i32, CompileError> {
    match expr {
        Node::Number(n) => Ok(n),
        Node::Address(_) | Node::ArrayRef(_) | Node::Call(_) => Err(CompileError(
            0,
            0,
            String::from("Constants must be statically defined"),
        )),

        Node::Name(n) => {
            if let Some(val) = scope.get(&n) {
                Ok(*val)
            } else {
                Err(CompileError(0, 0, format!("Unknown const {}", n)))
            }
        }

        Node::Expr(lhs, op, rhs) => {
            let lhs = eval_const(lhs.into(), scope)?;
            let rhs = eval_const(rhs.into(), scope)?;
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
        Node::Prefix(p, child) => {
            let val = eval_const(child.into(), scope)?;
            match p {
                Prefix::Neg => Ok(-val),
                Prefix::Not => {
                    if val == 0 {
                        Ok(1)
                    } else {
                        Ok(0)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval_const() {
        let empty_scope = ConstScope::new();
        let to_node = |s| Node::parse(s).unwrap();

        // Basic arithmetic
        assert_eq!(eval_const(to_node("2 * 3 + 4"), &empty_scope), Ok(10));
        assert_eq!(eval_const(to_node("1 + -2"), &empty_scope), Ok(-1));
        assert_eq!(eval_const(to_node("1 << 3"), &empty_scope), Ok(8));

        // Names
        let scope: ConstScope = [("foo".into(), 10), ("bar".into(), 5)].into();
        assert_eq!(eval_const(to_node("foo + 5"), &scope), Ok(15));
        assert_eq!(eval_const(to_node("bar * foo"), &scope), Ok(50));

        // Error
        assert_eq!(
            eval_const(to_node("nope"), &scope),
            Err(CompileError(0, 0, String::from("Unknown const nope")))
        );
    }

    #[test]
    fn test_const_decl() {
        let mut state = State::default();
        parse("const foo = 17 + 3;")
            .unwrap()
            .process(&mut state)
            .unwrap();
        assert_eq!(state.const_scope, [("foo".into(), 20)].into())
    }

    #[test]
    fn test_global_decl() {
        let mut state = State::default();
        parse("global a;").unwrap().process(&mut state).unwrap();
        assert_eq!(
            state.global_scope,
            [("a".into(), "_gensym_1".into())].into()
        )
    }

    #[test]
    fn test_name_collision() {
        let mut state = State::default();
        assert!(parse("const a = 7; global a;")
            .unwrap()
            .process(&mut state)
            .is_err());
    }
}
