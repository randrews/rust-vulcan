use crate::ast::*;

use std::collections::btree_map::Entry::Vacant;
use std::collections::{BTreeMap};
use std::fmt::{Display, Formatter};
use crate::forge_parser::{ParseError, parse};

#[derive(Eq, Clone, PartialEq, Debug)]
pub struct CompileError(usize, usize, String);

impl From<ParseError> for CompileError {
    fn from(value: ParseError) -> Self {
        Self(value.0, value.1, value.2)
    }
}

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
pub struct CompiledFn {
    pub label: Label,
    pub frame_size: usize,
    pub local_scope: Scope,
    pub body: Vec<String>,
}

impl CompiledFn {
    /// Add a name to the local scope, which:
    /// - Increases the size of the stack frame by that much
    /// - Records the offset into the local stack frame where that variable is stored
    fn add_local(&mut self, name: &str) -> Result<(), CompileError> {
        if let Vacant(e) = self.local_scope.entry(name.into()) {
            e.insert(Variable::Local(self.frame_size));
            self.frame_size += 3;
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

    /// The size of the local scope in bytes. This increases as variables are declared
    /// todo: this needs to change for arrays; it won't like having vars that aren't 3 bytes long
    fn frame_size(&self) -> usize {
        self.local_scope.len() * 3
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
    pub functions: BTreeMap<String, CompiledFn>,
    /// The string table
    pub strings: Vec<(Label, String)>,
    /// The functions that have been prototyped but not yet defined
    pub prototypes: Scope,
}

impl State {
    /// Generate a guaranteed-unique symbolic name
    fn gensym(&mut self) -> Label {
        self.gensym_index += 1;
        format!("_forge_gensym_{}", self.gensym_index)
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

    fn declare_function(&mut self, name: &str, _args: Vec<String>) -> Result<(), CompileError> {
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

    fn find_or_declare_function(&mut self, name: &str, loc: Location) -> Result<String, CompileError> {
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

trait Compilable {
    // Every AST node we visit can see the global compiler state (to make unique
    // symbols and reach for global names) as well as optionally the current
    // function (for AST nodes within functions). Additionally se send a Location:
    // This is the closest ancestor's location, in case we need to emit a compile
    // error
    fn process(
        self,
        state: &mut State,
        function: Option<&mut CompiledFn>,
        location: Location
    ) -> Result<(), CompileError>;
}

///////////////////////////////////////////////////////////

impl Compilable for Program {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        for decl in self.0 {
            decl.ast.process(state, None, decl.location)?
        }
        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Declaration {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        match self {
            Declaration::Function(f) => f.process(state, None, loc),
            Declaration::Global(g) => g.process(state, None, loc),
            Declaration::Const(c) => c.process(state, None, loc),
            Declaration::Prototype(p) => p.process(state, None, loc),
        }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Block {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Block outside function");

        // Compile each statement:
        for stmt in self.0 {
            let loc = stmt.location;
            match stmt.ast {
                Statement::Return(Return(None)) => {
                    // Returning nothing, so just default to returning a 0:
                    sig.emit_arg("ret", 0)
                }
                Statement::Return(Return(Some(expr))) => {
                    // Eval the expr and emit a ret for it
                    expr.process(state, Some(sig), loc)?;
                    sig.emit("ret")
                }
                Statement::Assignment(assign) => assign.process(state, Some(sig), loc)?,
                Statement::Expr(expr) => {
                    expr.process(state, Some(sig), loc)?;
                    // Every expr leaves a single-word return value on the stack. In an rvalue this
                    // is useful but in a statement it's garbage (because nothing else is about to
                    // pick it up) so, drop it:
                    sig.emit("pop")
                }
                Statement::VarDecl(vardecl) => vardecl.process(state, Some(sig), loc)?,
                Statement::Asm(Asm { args, body}) => {
                    // Process all the args, if any
                    for a in args {
                        (*a.0).process(state, Some(sig), loc)?
                    }
                    // Emit the body
                    sig.emit(body.as_str())
                }
                Statement::Conditional(Conditional { condition, body, alternative }) => {
                    condition.process(state, Some(sig), loc)?;
                    sig.emit("#if"); // We went to a lot of trouble making macros, shame not to use them
                    body.process(state, Some(sig), loc)?;
                    if let Some(alternative) = alternative {
                        sig.emit("#else");
                        alternative.process(state, Some(sig), loc)?;
                    }
                    sig.emit("#end")
                }
                Statement::WhileLoop(WhileLoop { condition, body }) => {
                    sig.emit("#while");
                    condition.process(state, Some(sig), loc)?;
                    sig.emit("#do");
                    body.process(state, Some(sig), loc)?;
                    sig.emit("#end")
                }
                Statement::RepeatLoop(RepeatLoop{ count, name: Some(name), body }) => {
                    // A repeat loop with a name essentially starts with a vardecl, so, we'll make
                    // a decl and process it (and deal with the error if it's a duplicate name)
                    // TODO: This makes some assumptions about names and scoping, and opens a can of
                    // worms. I think what we'd really like to do here is implement block scoping for real,
                    // which actually shouldn't be too hard: after a block, reset the scope to whatever
                    // it was before the block, and make the repeat loop counter go in its block.
                    let decl = VarDecl{
                        name: name.clone(),
                        size: None,
                        initial: Some(Expr::Number(0)),
                    };
                    decl.process(state, Some(sig), loc)?;

                    // Okay, the counter is now declared and in scope; we'll eval the limit once
                    count.process(state, Some(sig), loc)?;

                    // Now we have a fairly normal while loop:
                    sig.emit("#while");
                    // Stack currently has the limit on top. We need to cmp the counter to that.
                    sig.emit("dup");
                    // Load the counter
                    Expr::Name(name.clone()).process(state, Some(sig), loc)?;
                    // Subtract the counter from (the copy of) the limit.
                    sig.emit("sub");
                    // cmp that to zero, for our flag
                    sig.emit_arg("agt", 0);
                    // Loop body:
                    sig.emit("#do");
                    body.process(state, Some(sig), loc)?;
                    // After the body we need to increment the counter. Put its address on top:
                    Expr::Address(Expr::Name(name.clone()).into()).process(state, Some(sig), loc)?;
                    // Dup and load it:
                    sig.emit("dup");
                    sig.emit("loadw");
                    // Increment it:
                    sig.emit_arg("add", 1);
                    // Now we have ( counted-addr new-ctr-val ) so swap and store
                    sig.emit("swap");
                    sig.emit("storew");
                    // Back to the check!
                    sig.emit("#end");
                    // Done with the loop, but the limit is still on the stack, pop it:
                    sig.emit("pop");
                }
                Statement::RepeatLoop(RepeatLoop{ count, name: None, body }) => {
                    // No name, so, what we'll do is, eval the count:
                    count.process(state, Some(sig), loc)?;
                    // Now the count is on top of the stack, so we'll do a #while loop counting it
                    // down to zero:
                    sig.emit("#while");
                    sig.emit("dup");
                    // Only go through the loop if the counter is positive:
                    sig.emit_arg("agt", 0);
                    sig.emit("#do");
                    body.process(state, Some(sig), loc)?;
                    // After the body we need to decrement the counter:
                    sig.emit_arg("sub", 1);
                    // Back to the check!
                    sig.emit("#end");
                    // Loop is over so we're left with the dead counter on the top, drop it:
                    sig.emit("pop");
                }
            }
        }

        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Function {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let label = state.find_or_declare_function(self.name.as_str(), loc)?;

        // The CompiledFn for this function, which will eventually get stuff populated into it:
        let mut sig = CompiledFn {
            label,
            ..Default::default()
        };

        // Add each argument as a local
        for arg in self.args {
            sig.add_local(&arg)?
        }

        // todo we need to store arity somehow in the state, so we can check arglists even
        // with recursive calls. This probably becomes splitting "signature" from "function context"
        // and setting signature immutably right now, passing context down ("block?") and setting it
        // at the end

        // Compile the body, storing all of it in the CompiledFn we just created / added
        self.body.process(state, Some(&mut sig), loc)?;

        // This can't fail because if it were a dupe name, adding the global would have failed
        state.functions.insert(self.name.clone(), sig);

        // todo actually emit the function header / body
        Ok(())
    }
}

impl Compilable for FunctionPrototype {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        state.declare_function(self.name.as_str(), self.args)
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
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let Assignment { lvalue, rvalue } = self;
        let sig = sig.expect("Assignment outside function");
        // First the value, then the address we'll storew it to
        rvalue.process(state, Some(sig), loc)?;
        lvalue.process(state, Some(sig), loc)?;
        sig.emit("storew");
        Ok(())
    }
}

///////////////////////////////////////////////////////////

impl Compilable for VarDecl {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        let sig = sig.expect("Var declaration outside function");
        if self.size.is_some() {
            todo!("Arrays are not yet supported")
        }
        if let Some(initial) = self.initial {
            // If it's got an initial value, we have to compile that before we add
            // the name to scope, or else UB will ensue if it refers to itself:
            initial.process(state, Some(sig), loc)?;
            // But then add it to scope and assign:
            sig.add_local(&self.name)?;
            // We'll just whip up an lvalue real quick...
            Lvalue::from(self.name).process(state, Some(sig), loc)?;
            sig.emit("storew"); // And store the initial value there
        } else {
            // Otherwise, just add it to scope and leave garbage in there:
            sig.add_local(&self.name)?;
        }
        Ok(())
    }
}

///////////////////////////////////////////////////////////

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
            // Derefs are just evaluating the expr and leaving its value (an address) on the stack
            Expr::Deref(BoxExpr(expr)) => {
                (*expr).process(state, Some(sig), loc)
            }
            Expr::Subscript(_, _) => {
                Err(CompileError(0, 0, String::from("Arrays are not yet supported")))
            }
        }
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Call {
    fn process(self, state: &mut State, sig: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        // Require a function
        let sig = sig.expect("lvalue outside a function");

        // We need a stack consisting of the arguments (last on top), followed by the address to call
        // So, first eval the args:
        for arg in self.args {
            arg.process(state, Some(sig), loc)?;
        }

        // Eval the target
        self.target.0.process(state, Some(sig), loc)?;

        // Before we actually do the call though, we need to deal with some paperwork around the
        // frame pointer. We will store the current frame pointer in the rstack:
        sig.emit("loadw frame");
        sig.emit("pushr");

        // Now we increment the frame ptr to right after the current frame:
        let frame_size =  sig.frame_size();
        if frame_size > 0 {
            sig.emit("loadw frame");
            sig.emit_arg("add", frame_size);
            sig.emit("storew frame");
        }

        // Frame is now pointing at a safe place, the top of stack is the target, do the call:
        sig.emit("call");

        // And this is where we'll return to. The function has popped its args and left a word on
        // the stack as a return value, which someone else will deal with (this is the word that
        // this expr::call will end up evaluating to). But before we're done, we need to restore
        // our frame pointerS
        sig.emit("popr");
        sig.emit("storew frame");

        // And we're finished!
        Ok(())
    }
}

///////////////////////////////////////////////////////////

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
                        sig.emit("loadw frame");
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

///////////////////////////////////////////////////////////

impl Compilable for Global {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
        if self.size.is_some() {
            todo!("Arrays are not yet supported")
        }
        state.add_global(&self.name, |s| Variable::IndirectLabel(s.gensym()))
    }
}

///////////////////////////////////////////////////////////

impl Compilable for Const {
    fn process(self, state: &mut State, _: Option<&mut CompiledFn>, loc: Location) -> Result<(), CompileError> {
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
        Expr::Address(_) | Expr::Deref(_) | Expr::String(_) => Err(CompileError(
            0,
            0,
            String::from("Addresses are not known at compile time"),
        )),
        Expr::Call(_) | Expr::Subscript(_, _) => Err(CompileError(
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

///////////////////////////////////////////////////////////////////////////////////////////////////

/// Turn a `Program` into a list of assembly lines that can be assembled to run it.
/// There are various ways to build a program, this one is the simplest: it builds it as a
/// complete ROM that will place a jmp to main() at 0x400, so a Vulcan can boot from it.
pub fn build_boot(src: &str) -> Result<Vec<String>, CompileError> {
    let mut state = State::default();
    let ast = parse(src).map_err(CompileError::from)?;
    ast.process(&mut state, None, (0, 0).into())?;

    if let Some(Variable::DirectLabel(label)) = state.global_scope.get("main") {
        // Let's make a vec for the final listing
        let mut asm: Vec<String> = Vec::new();

        // Now we start piling stuff into the vec, starting with an org:
        asm.push(".org 0x400".into());

        // jmp into main:
        asm.push(format!("call {}", label));

        // When main returns, just hlt:
        asm.push("hlt".into());

        // Now start dumping compiled objects into there:
        for (label, val) in state.strings.iter() {
            asm.push(format!("{}: .db \"{}\\0\"", label, val))
        }
        for (_, val) in state.functions.iter_mut() {
            asm.push(format!("{}:", val.label));
            asm.append(val.body.as_mut());
        }

        // Final thing is to place a label for the stack:
        // (this is just a cell with the address of the following word)
        asm.push("frame: .db $+1".into());
        asm.push(".db 0".into());
        Ok(asm)
    } else {
        Err(CompileError(0, 0, "Function main not defined (or not a function)".into()))
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

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

    #[test]
    fn test_global_decl() {
        let mut state = State::default();
        parse("global a;")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .unwrap();
        assert_eq!(
            state.global_scope,
            [("a".into(), Variable::IndirectLabel("_forge_gensym_1".into()))].into()
        )
    }

    #[test]
    fn test_name_collision() {
        let mut state = State::default();
        assert!(parse("const a = 7; global a;")
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .is_err());
    }

    fn state_for(src: &str) -> State {
        let mut state = State::default();
        parse(src)
            .unwrap()
            .process(&mut state, None, (0, 0).into())
            .expect("Failed to compile");
        state
    }

    fn test_body(state: State) -> String {
        state.functions.get("test").unwrap().body.join("\n")
    }

    #[test]
    fn test_basic_fns() {
        assert_eq!(
            test_body(state_for("fn test(a, b) { b = 17 + a; }")),
            vec![
                "push 17",     // Start calculating the rvalue, push the literal
                "loadw frame", // This is looking up the "a" arg, at frame + 0
                "loadw",
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
        assert_eq!(
            test_body(state_for("fn test() { var a; var b = 7; a = b * 2; }")),
            vec![
                "push 7",      // Start calculating the rvalue, push the literal
                "loadw frame", // "b" is the second local var at frame + 3
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
        let mut state = state_for("const s1 = \"foo\"; fn test() { var x; x = \"bar\"; var y = \"norp\"; }");
        assert_eq!(
            state.strings,
            vec![
                ("_forge_gensym_1".into(), "foo".into()),
                ("_forge_gensym_3".into(), "bar".into()), // gensym 2 is the entrypoint of blah()
                ("_forge_gensym_4".into(), "norp".into())
            ]
        );
        assert_eq!(
            test_body(state),
            vec![
                "push _forge_gensym_3",
                "loadw frame",
                "storew", // the assignment for x
                "push _forge_gensym_4",
                "loadw frame",
                "add 3", // the address of y (frame + 3) and put gensym_4 in it
                "storew"
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
                "loadw frame",
                "storew",      // the assignment for x
                "loadw frame", // The addr of x
                "loadw frame",
                "add 3", // the address of y (frame + 3) and put the addr of x (frame) in it
                "storew"
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
                "loadw frame",
                "storew",      // the assignment for x
                "loadw frame", // Now we're compiling *x, so load x's value, which is 3
                "loadw",
                "loadw", // Then load the value at 3
                "push 1000", // Push the addr 1000, for the lvalue
                "storew" // Store whatever's at 3 to 1000
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_string_exprs() {
        assert_eq!(
            test_body(state_for("const foo = \"foo\"; fn test() { var x = \"bar\" + 3; }")),
            vec![
                "push _forge_gensym_3", // 1 is the label in the string table for "foo", 2 for "blah,"
                "push 3", // so 3 is the string "bar"
                "add", // Add 3 to that address
                "loadw frame", // Store it in the first var
                "storew"
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_asm_statements() {
        assert_eq!(
            test_body(state_for("fn test() { var x; asm(&x) { swap 12\nstorew } }")),
            vec![
                "loadw frame", // Push the addr of x
                "swap 12", // The asm body, which swaps 12 behind it and stores it there
                "storew"
            ]
                .join("\n")
        )
    }

    #[test]
    fn test_conditionals() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } }")),
            vec![
                "push 3",
                "loadw frame",
                "storew", // x = 3
                "loadw frame",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "loadw frame",
                "storew", // The branch, x = 1
                "#end"]
                .join("\n")
        );

        assert_eq!(
            test_body(state_for("fn test() { var x = 3; if (x > 2) { x = 1; } else { x = 7; } }")),
            vec![
                "push 3",
                "loadw frame",
                "storew", // x = 3
                "loadw frame",
                "loadw",
                "push 2",
                "agt", // The condition, x > 2
                "#if", // The if itself
                "push 1",
                "loadw frame",
                "storew", // The affirmative branch, x = 1
                "#else", // The alternative
                "push 7",
                "loadw frame",
                "storew", // x = 7
                "#end"]
                .join("\n")
        )
    }

    #[test]
    fn test_while_loops() {
        assert_eq!(
            test_body(state_for("fn test() { var x = 0; var c = 0; while (c < 10) { x = x + c; c = c + 1; } }")),
            vec![
                "push 0",
                "loadw frame",
                "storew", // var x = 0
                "push 0",
                "loadw frame",
                "add 3",
                "storew", // var c = 0
                "#while", // Start the loop
                "loadw frame",
                "add 3",
                "loadw",
                "push 10",
                "alt", // c > 10
                "#do", // Start loop body
                "loadw frame",
                "loadw",
                "loadw frame",
                "add 3",
                "loadw",
                "add",
                "loadw frame",
                "storew", // x = x + c
                "loadw frame",
                "add 3",
                "loadw",
                "push 1",
                "add",
                "loadw frame",
                "add 3",
                "storew", // c = c + 1
                "#end" // End the loop body
                ]
                .join("\n")
        );
    }

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

    #[test]
    fn test_calls() {
        assert_eq!(
            test_body(state_for("fn test(a, b) { test(2, 3); }")),
            vec![
                "push 2", // evaluating args, in order
                "push 3",
                "push _forge_gensym_1", // evaluating target (this fn)
                "loadw frame", // Store the frame ptr
                "pushr",
                "loadw frame", // Increment the frame ptr
                "add 6",
                "storew frame",
                "call", // Actually make the call
                "popr", // Restore the frame ptr
                "storew frame",
                "pop" // expr-as-statement drops the evaluated value
                ]
                .join("\n")
        );
    }

    #[test]
    fn test_returns() {
        assert_eq!(
            test_body(state_for("fn test(a) { return a + 3; }")),
            vec![
                "loadw frame", // Load a
                "loadw",
                "push 3", // Add 3
                "add",
                "ret", // Return that
            ]
                .join("\n")
        );

        assert_eq!(
            test_body(state_for("fn test(a) { if (a > 0) { return; } }")),
            vec![
                "loadw frame", // Load a
                "loadw",
                "push 0", // Compare to 0
                "agt",
                "#if", // If statement
                "ret 0", // Default return value, for an expr-less return
                "#end",
            ]
                .join("\n")
        );
    }

    #[test]
    fn test_repeat_loops() {
        // With a counter
        assert_eq!(
            test_body(state_for("fn test(a) { var x = 0; repeat(a) c { x = x + c; } return x; }")),
            vec![
                "push 0", // Create the 'x' var and store 0 in it
                "loadw frame",
                "add 3",
                "storew",
                "push 0", // Create the 'c' var and store 0 in it
                "loadw frame",
                "add 6",
                "storew",
                "loadw frame", // Load 'a' from args
                "loadw",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "loadw frame", // Load c
                "add 6",
                "loadw",
                "sub", // Subtract c from a
                "agt 0", // Are we still positive?
                "#do",
                "loadw frame", // Load x
                "add 3",
                "loadw",
                "loadw frame", // Load c
                "add 6",
                "loadw",
                "add", // Add c to x
                "loadw frame", // Load x as an lvalue
                "add 3",
                "storew", // Store c + x into it
                "loadw frame", // Load c as an lvalue
                "add 6",
                "dup", // Dup it, load it, add 1
                "loadw",
                "add 1",
                "swap", // Swap the addr on top and store it
                "storew",
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "loadw frame", // Load x so we can return it
                "add 3",
                "loadw",
                "ret"
            ]
                .join("\n")
        );

        // No counter
        assert_eq!(
            test_body(state_for("fn test(a) { var x = 1; repeat(a) { x = x * 2; } return x; }")),
            vec![
                "push 1", // Create the 'x' var and store 1 in it
                "loadw frame",
                "add 3",
                "storew",
                "loadw frame", // Load 'a' from args
                "loadw",
                "#while", // Starting the loop:
                "dup", // Copy the limit
                "agt 0", // Are we still positive?
                "#do",
                "loadw frame", // Load x
                "add 3",
                "loadw",
                "push 2", // double it
                "mul",
                "loadw frame", // Load x as an lvalue
                "add 3",
                "storew", // Store 2x into it
                "sub 1", // decrement the counter
                "#end", // End of the loop body!
                "pop", // Drop the limit off the top
                "loadw frame", // Load x so we can return it
                "add 3",
                "loadw",
                "ret"
            ].join("\n")
        );
    }

    #[test]
    fn test_build_boot() {
        // Very basic test of one main()
        let asm = build_boot("fn main() { return 5; }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "call _forge_gensym_1",
            "hlt",
            "_forge_gensym_1:",
            "push 5",
            "ret",
            "frame: .db $+1",
            ".db 0"
        ].join("\n"));

        // Slightly more complicated, with a global str
        let asm = build_boot("const str = \"blah\"; fn main() { return str; }".into()).unwrap();
        assert_eq!(asm.join("\n"), vec![
            ".org 0x400",
            "call _forge_gensym_2",
            "hlt",
            "_forge_gensym_1: .db \"blah\\0\"",
            "_forge_gensym_2:",
            "push _forge_gensym_1",
            "ret",
            "frame: .db $+1",
            ".db 0"
        ].join("\n"))
    }
}
