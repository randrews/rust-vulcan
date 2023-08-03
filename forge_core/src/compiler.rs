use crate::ast::*;

use std::collections::btree_map::Entry::Vacant;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};

#[derive(Eq, Clone, PartialEq, Debug)]
pub struct CompileError(usize, usize, String);

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.0, self.1, self.2)
    }
}

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

/// The data associated with a function signature:
/// - The frame size is in bytes
/// - The local scope is full of `Variable::Local`s, and contains args and local vars
/// - The body is just the statements of the function, not the code to move the frame
/// pointer around (which can't be generated until the variable declarations are all found)
///
/// A complete function implementation consists of:
/// - A label for the entrypoint
/// - Preamble code to set up the stack frame
/// - The function body
///
/// The stack frame is managed by the function through a global pointer called "frame". When
/// the function is called, it can assume that all memory after "frame" is free for use (this
/// isn't actually true because you can blow out the stack, but within reason it is). So when
/// you make a call to another function, you need to increment frame by the current frame size,
/// and then after the other function has returned, decrement it back so that frame again points
/// at your stack frame. Locals can be found by adding some offset from the frame pointer.
///
/// todo: the allocator problem has (probably) been solved! Make an alloca() that increases the
/// current frame pointer by some size. To dynamically allocate memory, just put it in the stack
/// frame of the current fn. All fns return one word and all params are one word long (structs
/// get passed around by reference)
///
/// todo: more of a global todo. Add a register that stores an offset that's added implicitly to
/// all absolute addresses. This makes it a lot simpler to make relocatable code
///
/// alternate todo: add a callr instruction. This moves the basic unit of linking up from "fn" to
/// "library". Near calls are callr, far calls are call. Globals, reserve a word in the zero page
/// for a global frame pointer, save / restore that around a far call (using the rstack).
///
/// Weird todo: add an abs (and maybe rel) instruction that converts a relative address on the
/// stack to an absolute one by adding the instruction pointer (of the abs / rel). Make room for it
/// by replacing sdp / setsdp / setint with just reg / setreg, which will push / pop register
/// values to the stack, given a register index (0 for data ptr, 1 for rstack, 2 for int enabled,
/// whatever). For a second removable opcode, how often is peekr actually used?
///
/// Another instruction todo: remove the copy instruction (currently a dumb copy-region command)
/// and replace it with the design from last year with the mode argument... but as a DMA "device"
/// with an interface in the zero page.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct Signature {
    pub label: Label,
    pub frame_size: usize,
    pub local_scope: Scope,
    pub body: Vec<String>,
}

impl Signature {
    /// Add a name to the local scope, which:
    /// - Increases the size of the stack frame by that much
    /// - Records the offset into the local stack frame where that variable is stored
    fn add_local(&mut self, name: &str) -> Result<(), CompileError> {
        if let Vacant(e) = self.local_scope.entry(name.into()) {
            e.insert(Variable::Local(self.frame_size));
            self.frame_size += 3; // todo: variously-sized structs
            Ok(())
        } else {
            Err(CompileError(0, 0, format!("Duplicate name {}", name)))
        }
    }

    /// Emit a string (ideally one instruction, but whatever) to the function body.
    /// This doesn't actually emit anything to output, the body will eventually be emitted
    /// in a final pass by the compiler once all functions are compiled.
    fn emit(&mut self, opcode: &str) {
        self.body.push(String::from(opcode))
    }

    /// A shorthand method to emit something with a `Display` arg, because emitting a
    /// single instruction with a variable (numeric or label) arg is very common.
    fn emit_arg<T: Display>(&mut self, opcode: &str, arg: T) {
        self.body.push(format!("{} {}", opcode, arg))
    }
}

/// Maps from names to the variables they represent
pub type Scope = BTreeMap<String, Variable>;

/// So we don't get confused between string-strings and assembly-label strings
pub type Label = String;

/// The compiler state:
#[derive(Clone, PartialEq, Debug, Default)]
struct State {
    /// Used by gensym to generate unique symbols
    pub gensym_index: usize,
    /// The globally-defined names
    pub global_scope: Scope,
    /// The functions
    pub functions: BTreeMap<String, Signature>,
    /// The string table
    pub strings: Vec<(Label, String)>,
}

impl State {
    /// Generate a guaranteed-unique symbolic name
    fn gensym(&mut self) -> Label {
        self.gensym_index += 1;
        format!("_gensym_{}", self.gensym_index)
    }

    /// Return whether a name exists in the global scope already
    fn defined(&self, name: &str) -> bool {
        self.global_scope.contains_key(name)
    }

    /// Add a symbol to the global namespace, catching name collisions
    fn add_global<F: Fn(&mut State) -> Variable>(
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

    fn add_string(&mut self, string: &str) -> Label {
        let sym = self.gensym();
        self.strings.push((sym.clone(), string.into()));
        sym
    }
}

trait Compilable {
    // Every AST node we visit can see the global compiler state (to make unique
    // symbols and reach for global names) as well as optionally the current
    // function (for AST nodes within functions)
    fn process(
        self,
        state: &mut State,
        function: Option<&mut Signature>,
    ) -> Result<(), CompileError>;
}

///////////////////////////////////////////////////////////

impl Compilable for Program {
    fn process(self, state: &mut State, _: Option<&mut Signature>) -> Result<(), CompileError> {
        for decl in self.0 {
            decl.process(state, None)?
        }
        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Declaration {
    fn process(self, state: &mut State, _: Option<&mut Signature>) -> Result<(), CompileError> {
        match self {
            Declaration::Function(f) => f.process(state, None),
            Declaration::Global(g) => g.process(state, None),
            Declaration::Struct(_) => todo!("Structs are not yet supported"),
            Declaration::Const(c) => c.process(state, None),
        }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Function {
    fn process(self, state: &mut State, _: Option<&mut Signature>) -> Result<(), CompileError> {
        // The signature for this function, which will eventually get added to the state
        let mut sig = Signature {
            label: state.gensym(),
            ..Default::default()
        };

        // Add each argument as a local
        for arg in self.args {
            if arg.typename.is_some() {
                todo!("Structs are not yet supported")
            }
            sig.add_local(&arg.name)?
        }

        // todo we need to store arity somehow in the state, so we can check arglists even
        // with recursive calls. This probably becomes splitting "signature" from "function context"
        // and setting signature immutably right now, passing context down ("block?") and setting it
        // at the end

        // Add it to the global namespace so we can make recursive calls
        state.add_global(&self.name, |_| Variable::DirectLabel(sig.label.clone()))?;

        // Compile each statement:
        for stmt in self.body.0 {
            match stmt {
                Statement::Return(_) => {}
                Statement::Assignment(assign) => assign.process(state, Some(&mut sig))?,
                Statement::Expr(expr) => {
                    todo!();
                    // expr.process(state, Some(&mut sig))?;
                    // // Every expr leaves a single-word return value on the stack. In an rvalue this
                    // // is useful but in a statement it's garbage (because nothing else is about to
                    // // pick it up) so, drop it:
                    // sig.emit("pop")
                }
                Statement::VarDecl(vardecl) => vardecl.process(state, Some(&mut sig))?,
                Statement::Conditional(_) | Statement::WhileLoop(_) | Statement::RepeatLoop(_) => {
                    todo!()
                }
            }
        }

        // This can't fail because if it were a dupe name, adding the global would have failed
        state.functions.insert(self.name.clone(), sig);

        Ok(())
    }
}

/// Look up a name first in the local scope, and failing that in the global scope.
fn lookup<'a>(name: &str, global_scope: &'a Scope, local_scope: &'a Scope) -> Option<&'a Variable> {
    if let Some(var) = local_scope.get(name) {
        Some(var)
    } else if let Some(var) = global_scope.get(name) {
        Some(var)
    } else {
        None
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Assignment {
    fn process(self, state: &mut State, sig: Option<&mut Signature>) -> Result<(), CompileError> {
        let Assignment { lvalue, rvalue } = self;
        let sig = sig.expect("Assignment outside function");

        // For a normal expr, eval and leave on the stack; for a string literal, add it to
        // the str table and push the label's address
        // match rvalue {
        //     Rvalue::Expr(rvalue) => rvalue.process(state, Some(sig))?,
        //     Rvalue::String(string) => {
        //         let label = state.add_string(&string);
        //         sig.emit_arg("push", label);
        //     }
        // }

        // Then process the lvalue and storew
        lvalue.process(state, Some(sig))?;
        sig.emit("storew");
        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for VarDecl {
    fn process(self, state: &mut State, sig: Option<&mut Signature>) -> Result<(), CompileError> {
        todo!();
        // let mut sig = sig.expect("Var declaration outside function");
        // if self.typename.is_some() || self.size.is_some() {
        //     todo!("Structs and arrays are not yet supported")
        // }
        // if let Some(initial) = self.initial {
        //     // If it's got an initial value, we have to compile that before we add
        //     // the name to scope, or else UB will ensue if it refers to itself:
        //     initial.process(state, Some(sig))?;
        //     // But then add it to scope and assign:
        //     sig.add_local(&self.name)?;
        //     // We'll just whip up an lvalue real quick...
        //     Lvalue::Name(self.name).process(state, Some(sig))?;
        //     sig.emit("storew"); // And store the initial value there
        // } else {
        //     // Otherwise, just add it to scope and leave garbage in there:
        //     sig.add_local(&self.name)?;
        // }
        // Ok(())
    }
}

///////////////////////////////////////////////////////////

/// Evaluate an lvalue and leave its address on the stack (ready to be consumed by storew)
impl Compilable for Lvalue {
    fn process(self, state: &mut State, sig: Option<&mut Signature>) -> Result<(), CompileError> {
        let global_scope = &state.global_scope;
        let mut sig = sig.expect("lvalue outside a function");
        match self {
            Lvalue::ArrayRef(_, _) => todo!("Arrays are not implemented yet"),
            Lvalue::Name(name) => {
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
                            sig.emit_arg("loadw", "frame");
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
        }
    }
}

///////////////////////////////////////////////////////////

/// Evaluate an expression in the context of a local scope. The runtime brother to eval_const.
/// This recursively evaluates a Node and leaves its value on the stack.
impl Compilable for Expr {
    fn process(self, state: &mut State, sig: Option<&mut Signature>) -> Result<(), CompileError> {
        todo!();
        // let mut sig = sig.expect("Non-const expression outside a function");
        // let global_scope = &state.global_scope;
        //
        // // First, a sanity check: try and eval_const this. If it's something incredibly basic
        // // that just becomes an i32, then we don't need to do anything else:
        // if let Ok(val) = eval_const(self.clone(), &state.global_scope) {
        //     sig.emit_arg("push", val);
        //     return Ok(());
        // }
        //
        // // Okay, looks like we need something that's in scope. Let's recurse:
        // match self.val {
        //     Val::Number(n) => {
        //         // Numbers are just pushed as literals
        //         sig.body.push(format!("push {}", n));
        //         Ok(())
        //     }
        //     Val::Name(name) => match lookup(&name, global_scope, &sig.local_scope) {
        //         // Names are treated differently depending on what they are
        //         Some(Variable::Literal(val)) => {
        //             // Names of constants are just that number
        //             sig.emit_arg("push", *val);
        //             Ok(())
        //         }
        //         Some(Variable::IndirectLabel(label)) => {
        //             // Names pointing at labels are loaded (rvalue; for lvalues they aren't)
        //             // Indirect labels are the address of where the value is stored (a var, .db)
        //             sig.emit_arg("loadw", label.clone());
        //             Ok(())
        //         }
        //         Some(Variable::DirectLabel(label)) => {
        //             // Direct labels are like functions, the label itself is the value, so just
        //             // push it:
        //             sig.emit_arg("push", label.clone());
        //             Ok(())
        //         }
        //         Some(Variable::Local(offset)) => {
        //             // Names of locals are added from the frame pointer
        //             let offset = *offset;
        //             sig.emit("loadw frame");
        //             if offset > 0 {
        //                 sig.emit_arg("add", offset);
        //                 sig.emit("loadw");
        //             }
        //             Ok(())
        //         }
        //         None => Err(CompileError(0, 0, format!("Unknown name {}", name))),
        //     },
        //     Val::Expr(node) => {
        //         todo!();
        //         // // Recurse on expressions, handling operators
        //         // lhs.0.process(state, Some(&mut sig))?;
        //         // rhs.0.process(state, Some(&mut sig))?;
        //         // match op {
        //         //     // Basic math
        //         //     Operator::Add => sig.emit("add"),
        //         //     Operator::Sub => sig.emit("sub"),
        //         //     Operator::Mul => sig.emit("mul"),
        //         //     Operator::Div => sig.emit("div"),
        //         //     Operator::Mod => sig.emit("mod"),
        //         //     Operator::And => {
        //         //         // Vulcan "and" is bitwise, so we need to flag-ify both args to make it logical
        //         //         sig.emit("gt 0");
        //         //         sig.emit("swap");
        //         //         sig.emit("gt 0");
        //         //         sig.emit("and");
        //         //     }
        //         //     Operator::Or => {
        //         //         // Same as and, flag-ify both args
        //         //         sig.emit("gt 0");
        //         //         sig.emit("swap");
        //         //         sig.emit("gt 0");
        //         //         sig.emit("or");
        //         //     }
        //         //     Operator::BitAnd => sig.emit("and"),
        //         //     Operator::BitOr => sig.emit("or"),
        //         //     Operator::Xor => sig.emit("xor"),
        //         //     Operator::Lt => sig.emit("alt"),
        //         //     Operator::Le => {
        //         //         // LE and GE are the inverses of GT and LT (arithmetic versions)
        //         //         sig.emit("agt");
        //         //         sig.emit("not");
        //         //     }
        //         //     Operator::Gt => sig.emit("agt"),
        //         //     Operator::Ge => {
        //         //         sig.emit("alt");
        //         //         sig.emit("not");
        //         //     }
        //         //     Operator::Eq => {
        //         //         sig.emit("xor");
        //         //         sig.emit("not");
        //         //     }
        //         //     Operator::Ne => sig.emit("xor"),
        //         //     Operator::Lshift => sig.emit("lshift"),
        //         //     Operator::Rshift => sig.emit("arshift"),
        //         // }
        //         // Ok(())
        //     }
        //     // Node::Prefix(prefix, node) => {
        //     //     node.0.process(state, Some(sig))?;
        //     //     match prefix {
        //     //         Prefix::Neg => {
        //     //             // To arithmetically negate something, invert and increment (2s complement)
        //     //             sig.emit("xor -1");
        //     //             sig.emit("add 1");
        //     //         }
        //     //         Prefix::Not => sig.emit("not"),
        //     //     }
        //     //     Ok(())
        //     // }
        // }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Global {
    fn process(self, state: &mut State, _: Option<&mut Signature>) -> Result<(), CompileError> {
        if self.typename.is_some() || self.size.is_some() {
            todo!("Structs and arrays are not yet supported")
        }
        state.add_global(&self.name, |s| Variable::IndirectLabel(s.gensym()))
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Const {
    fn process(self, state: &mut State, _: Option<&mut Signature>) -> Result<(), CompileError> {
        todo!()
        // let var = if self.string.is_some() {
        //     // If it's a string, add it to the string table
        //     Variable::DirectLabel(state.add_string(&self.string.unwrap()))
        // } else if let Some(expr) = self.value {
        //     // Otherwise eval_const it
        //     Variable::Literal(eval_const(expr, &state.global_scope)?)
        // } else {
        //     unreachable!()
        // };
        // // Add it to the global namespace
        // state.add_global(&self.name, |_| var.clone())
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
pub fn eval_const(expr: Expr, scope: &Scope) -> Result<i32, CompileError> {
    todo!()
    // match expr.lhs {
    //     Val::Number(n) => Ok(n),
    //     // Node::Address(_) | Node::ArrayRef(_) | Node::Call(_) => Err(CompileError(
    //     //     0,
    //     //     0,
    //     //     String::from("Constants must be statically defined"),
    //     // )),
    //
    //     Val::Name(n) => {
    //         if let Some(Variable::Literal(val)) = scope.get(&n) {
    //             Ok(*val)
    //         } else {
    //             Err(CompileError(0, 0, format!("Unknown const {}", n)))
    //         }
    //     }
    //
    //     Val::Expr(node) => {
    //         todo!()
    //         // let lhs = eval_const(lhs.into(), scope)?;
    //         // let rhs = eval_const(rhs.into(), scope)?;
    //         // match op {
    //         //     Operator::Add => Ok(lhs + rhs),
    //         //     Operator::Sub => Ok(lhs - rhs),
    //         //     Operator::Mul => Ok(lhs * rhs),
    //         //     Operator::Div => Ok(lhs / rhs),
    //         //     Operator::Mod => Ok(lhs % rhs),
    //         //     Operator::And => Ok(to_flag(lhs != 0 && rhs != 0)),
    //         //     Operator::Or => Ok(to_flag(lhs != 0 || rhs != 0)),
    //         //     Operator::BitAnd => Ok(lhs & rhs),
    //         //     Operator::BitOr => Ok(lhs | rhs),
    //         //     Operator::Xor => Ok(lhs ^ rhs),
    //         //     Operator::Lt => Ok(to_flag(lhs < rhs)),
    //         //     Operator::Le => Ok(to_flag(lhs <= rhs)),
    //         //     Operator::Gt => Ok(to_flag(lhs > rhs)),
    //         //     Operator::Ge => Ok(to_flag(lhs >= rhs)),
    //         //     Operator::Eq => Ok(to_flag(lhs == rhs)),
    //         //     Operator::Ne => Ok(to_flag(lhs != rhs)),
    //         //     Operator::Lshift => Ok(lhs << rhs),
    //         //     Operator::Rshift => Ok(lhs >> rhs),
    //         // }
    //     }
    //     // Node::Prefix(p, child) => {
    //     //     let val = eval_const(child.into(), scope)?;
    //     //     match p {
    //     //         Prefix::Neg => Ok(-val),
    //     //         Prefix::Not => {
    //     //             if val == 0 {
    //     //                 Ok(1)
    //     //             } else {
    //     //                 Ok(0)
    //     //             }
    //     //         }
    //     //     }
    //     // }
    // }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::forge_parser::parse;

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
            .process(&mut state, None)
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("foo".into(), Variable::Literal(20))].into()
        )
    }

    #[test]
    fn test_global_decl() {
        let mut state = State::default();
        parse("global a;")
            .unwrap()
            .process(&mut state, None)
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("a".into(), Variable::IndirectLabel("_gensym_1".into()))].into()
        )
    }

    #[test]
    fn test_name_collision() {
        let mut state = State::default();
        assert!(parse("const a = 7; global a;")
            .unwrap()
            .process(&mut state, None)
            .is_err());
    }

    fn body_as_string(sig: &Signature) -> String {
        sig.body.join("\n")
    }

    #[test]
    fn test_basic_fns() {
        let mut state = State::default();
        parse("fn blah(a, b) { b = 17 + a; }")
            .unwrap()
            .process(&mut state, None)
            .expect("Failed to compile");
        let body = body_as_string(state.functions.get("blah").unwrap());
        assert_eq!(
            body,
            vec![
                "push 17",     // Start calculating the rvalue, push the literal
                "loadw frame", // This is looking up the "a" arg, at frame + 0
                "add",         // 17 + a
                "loadw frame", // Calculate the lvalue
                "add 3",       // "b" arg is frame + 3
                "storew",      // Finally store
            ]
            .join("\n")
        )
    }

    #[test]
    fn test_var_decls() {
        let mut state = State::default();
        parse("fn blah() { var a; var b = 7; a = b * 2; }")
            .unwrap()
            .process(&mut state, None)
            .expect("Failed to compile");
        let body = body_as_string(state.functions.get("blah").unwrap());
        assert_eq!(
            body,
            vec![
                "push 7",      // Start calculating the rvalue, push the literal
                "loadw frame", // "b" is the second local var at frame - 3
                "add 3",
                "storew",      // Do the initialization
                "loadw frame", // Now we're evaluating b * 2
                "add 3",       // b is at frame + 3...
                "loadw",       // load it to the stack
                "push 2",
                "mul",         // b * 2 evaluated
                "loadw frame", // Loading "a" as an lvalue
                "storew",      // doing the assignment
            ]
            .join("\n")
        )
    }

    #[test]
    fn test_literal_strings() {
        let mut state = State::default();
        parse("const s1 = \"foo\"; fn blah() { var x; x = \"bar\"; }")
            .unwrap()
            .process(&mut state, None)
            .expect("Failed to compile");
        let body = body_as_string(state.functions.get("blah").unwrap());
        assert_eq!(
            state.strings,
            vec![
                ("_gensym_1".into(), "foo".into()),
                ("_gensym_3".into(), "bar".into()) // gensym 2 is the entrypoint of blah()
            ]
        );
        assert_eq!(
            body,
            vec!["push _gensym_3", "loadw frame", "storew"].join("\n")
        ) // todo we want to allow strings in initializers also
    }
}
