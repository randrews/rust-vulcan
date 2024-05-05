use std::cmp::max;
use std::collections::btree_map::Entry::Vacant;
use std::fmt::Display;
use crate::compiler::compile_error::CompileError;
use crate::compiler::text::Text;
use crate::compiler::utils::{Label, Scope, Variable};

/// The data associated with a function signature:
/// - The frame size is in bytes
/// - The local scope is full of `Variable::Local`s, and contains args and local vars
/// - The body is just the statements of the function, not the code to move the frame
/// pointer around (which can't be generated until the variable declarations are all found)
///
/// A complete function implementation consists of:
/// - A label for the entrypoint
/// - A label for the outro (returns jmpr here)
/// - The function body
/// - The function preamble (code to set up the stack frame, capture args)
/// - The function outro (tear down the stack frame)
///
/// The stack frame is managed by the function through a pointer passed to it. When
/// the function is called, it can assume that all memory after that pointer is free for use (this
/// isn't actually true because you can blow out the stack, but within reason it is). So when
/// you make a call to another function, you need to increment frame by the current frame size,
/// and put that on the top of the data stack. Functions store their pointer in the top of the
/// rstack, and locals can be found by adding some offset from the frame pointer.
///
/// The max_frame_size field is important: this is the most entries that can be in scope at one time.
/// The reason that's important is that anything in the stack _after_ that many slots is available
/// for use by the allocator pool. The second entry in the rstack is the "pool pointer" which is the
/// address of the first byte of the stack that we haven't used yet. When we want to allocate more
/// memory for any reason (like, new(), or calling a fn and giving it a valid frame ptr), this is
/// what we pass.
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
    pub comments: bool,
    pub label: Label,
    pub end_label: Label,
    pub max_frame_size: usize,
    pub local_scope: Scope,
    pub arity: usize,
    pub continue_points: Vec<Option<Label>>,

    /// The preamble is the area of the function capturing the args, etc
    pub preamble: Text,

    /// The body is the actual body of the function
    pub body: Text,

    /// The outro handles restoring the stack / pool pointers and pushing the return value
    pub outro: Text,
}

impl CompiledFn {
    /// Add a name to the local scope, which:
    /// - Increases the size of the stack frame by that much
    /// - Records the offset into the local stack frame where that variable is stored
    pub(crate) fn add_local(&mut self, name: &str) -> Result<(), CompileError> {
        let frame = self.frame_size();
        if let Vacant(e) = self.local_scope.entry(name.into()) {
            e.insert(Variable::Local(frame));
            let frame = self.frame_size();
            self.max_frame_size = max(frame, self.max_frame_size);
            Ok(())
        } else {
            Err(CompileError(0, 0, format!("Duplicate name {}", name)))
        }
    }

    pub fn enter_loop(&mut self, continue_point: Option<Label>) {
        self.continue_points.push(continue_point)
    }
    pub fn exit_loop(&mut self) {
        if self.continue_points.len() > 0 {
            self.continue_points.pop();
        } else {
            // I doubt it's possible for this to happen, since it would be
            // caught at the parse stage
            unreachable!()
        }
    }
    pub fn in_loop(&self) -> bool {
        self.continue_points.len() > 0
    }
    pub fn continue_point(&self) -> &Option<Label> {
        self.continue_points.last().unwrap_or(&None)
    }

    /// Return an iterator over all the
    pub fn text(&self) -> impl Iterator<Item = &str> {
        self.preamble.0.iter().chain(self.body.0.iter()).chain(self.outro.0.iter()).map(String::as_str)
    }

    /// Emit and emit arg should just be delegated to `body`
    pub(crate) fn emit(&mut self, opcode: &str) {
        self.body.emit(opcode)
    }

    pub(crate) fn emit_arg<T: Display>(&mut self, opcode: &str, arg: T) {
        self.body.emit_arg(opcode, arg)
    }

    pub(crate) fn emit_comment<T: Display>(&mut self, arg: T) {
        if self.comments {
            self.body.emit(format!("{}", arg).as_str())
        }
    }

    /// The (current) size of the local scope in bytes. This increases as variables are declared,
    /// decreases as they leave scope
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
        // First, we need a pool pointer on the rstack. We know this because it's the current top
        // (the frame ptr) plus a (known) max scope size:
        self.preamble.emit("dup");
        if self.max_frame_size > 0 {
            // Pool ptr is right after the locals, so, add max_frame_size
            self.preamble.emit_arg("add", self.max_frame_size);
        }
        self.preamble.emit("pushr");

        // The top argument we were sent is the frame ptr, store that also:
        self.preamble.emit("pushr");

        // Add each argument as a local
        let mut arg_names: Vec<_> = args.iter().map(|a| a.clone()).collect();

        // Arguments in calls are pushed with the last on top, so, reverse the vec just to make
        // the following loop easier:
        arg_names.reverse();

        for name in arg_names {
            if let Variable::Local(offset) = self.local_scope[&name] {
                self.preamble.emit("peekr");
                if offset != 0 {
                    self.preamble.emit_arg("add", offset);
                }
                self.preamble.emit("storew");
            }
        }

        // Create the outro:
        // First, we might fall through to here, so, leave a push 0 on the stack. Normally we roll
        // with an empty stack, or have some data and jmpr here, but if we fall through this will
        // ensure that the following return returns something:
        self.outro.emit("push 0");

        // Now, the outro label:
        self.outro.emit(format!("{}:", self.end_label).as_str());

        // The top of the rstack is, of course, the frame ptr and pool ptr. So we need to get rid of
        // those:
        self.outro.emit("popr");
        self.outro.emit("pop");
        self.outro.emit("popr");
        self.outro.emit("pop");

        // We're in the same condition we entered in except that our return value is on the stack
        // (or a default 0 is) so time to actually return:
        self.outro.emit("ret");

        Ok(())
    }

    /// Takes a new frame size and removes all names from the local scope that would be placed
    /// beyond that frame length: used when compiling blocks to de-scope names that should only
    /// be visible in the block
    pub(crate) fn reduce_frame_size_to(&mut self, new_size: usize) {
        let mut to_remove: Vec<String> = Vec::new();
        for (name, var) in self.local_scope.iter() {
            if let Variable::Local(offset) = var {
                if *offset >= new_size {
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
        self.reduce_frame_size_to(self.frame_size() - 3);
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::test_utils::*;

    #[test]
    fn max_frame_size_args() {
        assert_eq!(test_preamble(state_for("fn test(a) { return 5; }")),
            vec![
                "dup", // ( frame frame )
                "add 3", // add max size to frame to get ( frame pool )
                "pushr", // save pool ptr
                "pushr", // save frame ptr
                "peekr", // store param in 'a'
                "storew",
            ].join("\n")
        )
    }

    #[test]
    fn max_frame_size_locals() {
        assert_eq!(test_preamble(state_for("fn test() { var x; return 5; }")),
                   vec![
                       "dup", // ( frame frame )
                       "add 3", // add max size to frame to get ( frame pool )
                       "pushr", // save pool ptr
                       "pushr", // save frame ptr
                   ].join("\n")
        )
    }

    #[test]
    fn max_frame_size_block_scope() {
        assert_eq!(test_preamble(state_for("fn test(p) { var x; if(3) { var y; } var z; }")),
                   vec![
                       "dup", // ( frame frame )
                       // This is the test: p and x are always in scope; y is created but leaves
                       // scope before z enters so z can reuse y's slot
                       "add 9", // add max size to frame to get ( frame pool )
                       "pushr", // save pool ptr
                       "pushr", // save frame ptr
                       "peekr", // Capture param p
                       "storew",
                   ].join("\n")
        )
    }
}