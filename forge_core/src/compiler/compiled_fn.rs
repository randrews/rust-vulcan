use std::collections::btree_map::Entry::Vacant;
use std::fmt::Display;
use crate::ast::Function;
use crate::compiler::compile_error::CompileError;
use crate::compiler::utils::{Label, Scope, Variable};

/// The data associated with a function signature:
/// - The frame size is in bytes
/// - The local scope is full of `Variable::Local`s, and contains args and local vars
/// - The body is just the statements of the function, not the code to move the frame
/// pointer around (which can't be generated until the variable declarations are all found)
///
/// A complete function implementation consists of:
/// - A label for the entrypoint
/// - The function body (including preamble code to set up the stack frame)
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
    pub arity: usize,
    pub alloc: bool,
    pub preamble: Vec<String>,
    pub body: Vec<String>,
    pub outro: Vec<String>,
}

impl CompiledFn {
    /// Add a name to the local scope, which:
    /// - Increases the size of the stack frame by that much
    /// - Records the offset into the local stack frame where that variable is stored
    pub(crate) fn add_local(&mut self, name: &str) -> Result<(), CompileError> {
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
    pub(crate) fn emit(&mut self, opcode: &str) {
        self.body.push(String::from(opcode))
    }

    /// Emit a string (ideally one instruction, but whatever) to the function preamble.
    /// This is very similar to `emit` but it appends the line to a section that will be written
    /// before the body. If we need to write something that depends on the body but must be run
    /// before it, it gets written through here.
    pub(crate) fn preamble_emit(&mut self, opcode: &str) {
        self.preamble.push(String::from(opcode))
    }

    /// Emit a string (ideally one instruction, but whatever) to the function outro. Just like the
    /// body and preamble, the outro is part of the fn, but will be emitted after the fn body. Any
    /// cleanup that needs to happen should happen here.
    pub(crate) fn outro_emit(&mut self, opcode: &str) {
        self.outro.push(String::from(opcode))
    }

    /// A shorthand method to emit something with a `Display` arg, because emitting a
    /// single instruction with a variable (numeric or label) arg is very common.
    pub(crate) fn emit_arg<T: Display>(&mut self, opcode: &str, arg: T) {
        self.body.push(format!("{} {}", opcode, arg))
    }

    /// Preamble version of emit_arg
    pub(crate) fn preamble_emit_arg<T: Display>(&mut self, opcode: &str, arg: T) {
        self.preamble.push(format!("{} {}", opcode, arg))
    }

    /// The size of the local scope in bytes. This increases as variables are declared
    /// todo: this needs to change for arrays; it won't like having vars that aren't 3 bytes long
    pub(crate) fn frame_size(&self) -> usize {
        self.local_scope.len() * 3
    }

    /// We won't generate the code to actually capture the args until we generate the preamble
    /// (which we can't do until we compile the body) but we need to do some recognition of the
    /// args here: count how many there are, add them to the scope, so we know how to refer to them.
    pub(crate) fn calculate_arity(&mut self, args: &Vec<String>) -> Result<(), CompileError> {
        // Add each argument as a local
        for name in args.iter() {
            self.add_local(name.as_str())?;
            self.arity += 1;
        }
        Ok(())
    }

    /// Emit the code to add the arguments' names to the local scope, and move the arguments off
    /// the stack into the frame. Should be run after the body is compiled, because this code
    /// depends on knowledge of the body of the fn, but what this produces will be emitted to
    /// the final listing before the fn body
    pub(crate) fn generate_preamble_outro(&mut self, args: &Vec<String>) -> Result<(), CompileError> {
        let mut arg_names: Vec<&str> = Vec::new();

        // Does our fn make any allocations? If so we need to save the old alloc pool pointer:
        if self.alloc {
            todo!("alloc pool not actually implemented");
            self.preamble_emit("loadw pool");
            self.preamble_emit("pushr");
        }

        // Top argument is the frame ptr; copy it to the rstack:
        self.preamble_emit("pushr");

        // Add each argument as a local
        let mut arg_names: Vec<_> = args.iter().map(|a| a.clone()).collect();

        // Arguments in calls are pushed with the last on top, so, reverse the vec just to make
        // the following loop easier:
        arg_names.reverse();

        for name in arg_names {
            if let Variable::Local(offset) = self.local_scope[&name] {
                self.preamble_emit("peekr");
                if offset != 0 {
                    self.preamble_emit_arg("add", offset);
                }
                self.preamble_emit("storew");
            }
        }

        Ok(())
    }

    /// Takes a new frame size and removes all names from the local scope that would be placed
    /// beyond that frame length: used when compiling blocks to de-scope names that should only
    /// be visible in the block
    pub(crate) fn reduce_frame_size_to(&mut self, new_size: usize) {
        self.frame_size = new_size;
        let mut to_remove: Vec<String> = Vec::new();
        for (name, var) in self.local_scope.iter() {
            if let Variable::Local(offset) = var {
                if *offset >= self.frame_size {
                    to_remove.push(name.clone());
                }
            }
        }

        for name in to_remove {
            self.local_scope.remove(name.as_str());
        }
    }

    /// Reduces the frame size by one word, to throw out the most recently declared local.
    /// Used for things like repeat loops where there's a name declared outside a block but which
    /// should only be visible in the block anyway.
    pub(crate) fn forget_last_local(&mut self) {
        self.reduce_frame_size_to(self.frame_size - 3);
    }
}
