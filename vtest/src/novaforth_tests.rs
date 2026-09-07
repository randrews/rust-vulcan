use memory_item::{ascii, heap, inst1, inst4, num, op, ptr, skip};
use crate::constants::SYMBOLS;
use crate::memory_item;
use crate::memory_item::PointerTarget;
use crate::test_harness::{init_cpu, TestHarness};

/// TODO:
/// - `quit` should clear the rstack but not the data stack, new opcode probably
/// - refactor test assert fns to be shorter / in a different file
///
/// Later TODO:
/// - Remove 'continue', we can implement it ourselves easily
/// - Prelude of simple words
/// - Rewrite / macro-ize string fns

#[test]
fn test_dupnz() {
    init_cpu().given_stack([3]).test_fn("dupnz").expect_stack([3, 3]);
    init_cpu().given_stack([0]).test_fn("dupnz").expect_stack([0]);

    init_cpu().test_line("10 ?dup").expect_stack([10, 10]);
    init_cpu().test_line("0 ?dup").expect_stack([0]);
}

#[test]
fn test_number_parsing() {
    init_cpu().test_line("10").expect_stack([10]);
    init_cpu().test_line("10 20 30").expect_stack([10, 20, 30]).expect_empty_rstack();
}

#[test]
fn test_lookup_fail() {
    init_cpu().test_line("notaword").expect_output("Not a word: notaword\n");
}

#[test]
fn test_create() {
    // Should create a new dictionary entry:
    init_cpu().test_line("create blah").expect_heap([
        ascii("blah"), // Name
        ptr(memory_item::new_heap()), // Points to right after the entry
        ptr("dict_start") // Points to the old dict head
    ]).expect_pointer("dictionary", heap(0)); // Dict has the new entry consed on to it
}

#[test]
fn entering_exiting_immediate_mode() {
    init_cpu().test_line("]").expect_pointer("handleword_hook", "compile_handleword");
    init_cpu().test_line("] [").expect_pointer("handleword_hook", "immediate_handleword").expect_empty_rstack();
}

#[test]
fn basic_compilation() {
    // Compiling a number
    init_cpu().test_line("] 122773").expect_heap([
        inst4("push", 122773)
    ]);

    // Compiling a call
    init_cpu().test_line("] create").expect_heap([
        inst4("call", ptr("nova_create"))
    ]);

    // Compiling gibberish
    init_cpu().test_line("] stillnotaword").expect_output("Not a word: stillnotaword\n");
}

#[test]
fn test_continue() {
    // Continue word (compiles a jmp)
    init_cpu().test_line("] continue ]").expect_heap([
        inst4("jmp", ptr("nova_close_bracket"))
    ]);

    // Continue compile word
    init_cpu().test_line("] continue [").expect_heap([
        inst4("jmp", ptr("nova_open_bracket"))
    ]);

    // Continue gibberish
    init_cpu().test_line("] continue supernotword").expect_output("Not a word: supernotword\n");

    // Implement continue with #asm!
    init_cpu()
        .test_line(": cont ' $ jmp #asm ; immediate")
        .test_line("] cont ]")
        .expect_heap([
            skip(11), // Just skip cont's header
            inst4("call", ptr("nova_tick")), // Call tick to see what we're continuing to
            inst4("push", op("jmp")), // Push a jmp
            inst4("call", ptr("compile_instruction_arg")), // Compile a jmp to that word
            inst1("ret"), // Return from cont
            inst4("jmp", ptr("nova_close_bracket")), // Cont gives us a jmp to `]`
        ]);

    // Prelude continue with a runtime word
    init_cpu()
        .test_line(": cont ' $ jmp #asm ; immediate")
        .test_line("] cont print")
        .expect_heap([
            skip(11 + 13), // Just skip cont's header and impl
            inst4("jmp", ptr("print")) // Cont gives us a jmp to `print`
        ]);
}

#[test]
fn test_prelude_colon() {
    // Prelude colon definition
    init_cpu()
        .test_line(": cont ' $ jmp #asm ; immediate")
        .test_line("create :: ] create cont ] [")
        .expect_heap([
            skip(24), // 24 bytes for cont
            ascii("::"), // New dict entry has the name
            ptr(heap(24 + 9)), // Followed by the ptr to the fn
            skip(3), // Pointer to dict start
            inst4("call", ptr("nova_create")), // Which is a call to create...
            inst4("jmp", ptr("nova_close_bracket")), // Followed by jmping to close_bracket
        ])
        .expect_pointer("handleword_hook", "immediate_handleword") // And now we're back in immediate mode
        .expect_empty_rstack(); // And haven't leaked a stack frame

    init_cpu()
        .test_line("create cont ] ' $ jmp #asm ; immediate")
        .test_line("create :: ] create cont ] [ :: foo 35")
        .expect_heap([
            skip(24 + 17), // Skip cont and ::
            ascii("foo"), // A new entry for foo
            ptr(heap(24 + 27)), // Defn ptr is right after this
            skip(3),
            inst4("push", 35), // fn begins with pushing a 35
        ])
        .expect_pointer("handleword_hook", "compile_handleword")
        .expect_empty_rstack();
}

#[test]
fn test_postpone() {
    // Postponing normal words
    init_cpu().test_line("] postpone create").expect_heap([
        inst4("push", ptr("nova_create")),
        inst4("push", op("call")),
        inst4("call", ptr("compile_instruction_arg"))
    ]);

    // Postponing compile words
    init_cpu().test_line("] postpone [").expect_heap([
        inst4("call", ptr("nova_open_bracket")),
    ]);

    // Postponing gibberish
    init_cpu().test_line("] postpone reallynotaword").expect_output("Not a word: reallynotaword\n");
}

#[test]
fn test_exit() {
    // Compile a ret
    init_cpu().test_line("] exit").expect_heap([
        inst1("ret")
    ]);
}

/// This was a fun intellectual exercise and makes a nice torture test for NovaForth, but it violates the
/// "optimize for understandability" principle and so colon and semicolon are now both written in asm. The
/// tests remain here because they're good, very exhaustive, tests.
#[test]
fn test_prelude() {
    // Implementing colon and semicolon in Forth itself
    let p1 = "create :: ] create continue ] [";
    let p2 = ":: ;; postpone exit continue [ [ immediate";
    let psize = 34;

    // Prelude semicolon definition
    init_cpu().test_line(p1).test_line(p2)
        .expect_heap([
            skip(17), ascii(";;"), skip(3), ptr("compile_dict_start"), // A new entry for semicolon
            inst4("call", ptr("nova_exit")), // Which compiles a ret
            inst4("jmp", ptr("nova_open_bracket")), // And then returns to immediate mode
        ])
        .expect_pointer("compile_dictionary", PointerTarget::Heap(17.into())) // Semicolon is in the compile dict
        .expect_pointer("handleword_hook", "immediate_handleword"); // In immediate mode again

    // Using prelude semicolon
    init_cpu().test_line(p1).test_line(p2).test_line("] ;;")
        .expect_heap([
            skip(psize),
            inst1("ret") // Compiled our ret
        ])
        .expect_pointer("handleword_hook", "immediate_handleword") // In immediate mode again
        .expect_empty_rstack();

    // Defining a word and calling it, with the prelude
    init_cpu().test_line(p1).test_line(p2).test_line(":: fives 5 5 5 ;; fives")
        .expect_stack([5, 5, 5])
        .expect_empty_rstack();

    // Testing create / does> without compile-time behavior, with the prelude
    init_cpu().test_line(p1).test_line(p2).test_line(":: blah create does> 2 3 ;; blah fnord fnord")
        // We're creating a new word fnord and then running it, the new word gets passed the address
        // of its heap stuff and then pushes a couple numbers. Its heap area is the heap ptr when we
        // called does>, so, PRELUDE + 11 (blah's entry) + 21 (blah's body, part of which is fnord's) + 12 (fnord's entry)
        .expect_stack([u32::from(SYMBOLS["heap_start"]) + psize + 11 + 22 + 12, 2, 3])
        .expect_heap([
            skip(psize + 11), // Skip prelude and blah's header
            // Body of blah:
            inst4("call", ptr("nova_create")), // After blah's header, we have a call to create
            inst4("push", ptr(heap(psize + 11 + 13))), // push the address of after the does>
            inst4("jmp", ptr("does_at_runtime")), // And a call to does@runtime, to start compiling it
            inst1("ret"), // blah's return
            inst4("push", 2), // The runtime behavior of fnord (the "mold"):
            inst4("push", 3),
            inst1("ret"), // fnord's runtime return

            // Header of fnord:
            ascii("fnord"), // the new word's header
            ptr(heap(psize + 11 + 22 + 12)), // pointer to the trampoline
            // and pointer to the next dictionary entry. By this point the front of the
            // dictionary is blah, which has its entry at heap(psize), right after the prelude:
            ptr(heap(psize)),

            // Body (trampoline) of fnord:
            // Push the old value, which was right after the header (because of the null compile-time behavior)
            inst4("push", ptr(heap(psize + 11 + 22 + 12))),
            inst4("jmp", ptr(heap(psize + 11 + 13))) // jmp to the runtime behavior, after the does> call
        ]);

    // Testing create / does> when there's compile-time behavior, with the prelude
    init_cpu().test_line(p1).test_line(p2).test_line(":: blah create 15 , does> 3 ;; blah fnord fnord")
        // We're creating a new word fnord and then running it, the new word gets passed the address
        // of its heap stuff and then pushes a three. Its heap area is the heap ptr when we
        // called does>, so, psize + 11 (blah's entry) + 26 (blah's body, part of which is fnord's) + 12 (fnord's entry)
        .expect_stack([u32::from(SYMBOLS["heap_start"]) + psize + 11 + 26 + 12, 3])
        .expect_heap([
            skip(psize + 11), // Skip prelude and blah's header
            // Body of blah:
            inst4("call", ptr("nova_create")), // After blah's header, we have a call to create
            inst4("push", 15),
            inst4("call", ptr("nova_comma")),
            inst4("push", ptr(heap(psize + 11 + 21))), // push the address of after the does>
            inst4("jmp", ptr("does_at_runtime")), // And a call to does@runtime, to start compiling it
            inst1("ret"), // blah's return
            inst4("push", 3), // The runtime behavior of fnord (the "mold"):
            inst1("ret"), // fnord's runtime return

            // Header of fnord:
            ascii("fnord"), // the new word's header
            ptr(heap(psize + 11 + 26 + 15)), // pointer to the trampoline
            // and pointer to the next dictionary entry. By this point the front of the
            // dictionary is blah, which has its entry at heap(psize), right after the prelude:
            ptr(heap(psize)),

            num(15), // The compile time behavior compiled this 15

            // Body (trampoline) of fnord:
            // Push the old value, which was right after the header and the 15 we compiled
            inst4("push", ptr(heap(psize + 11 + 26 + 12))),
            inst4("jmp", ptr(heap(psize + 11 + 21))) // jmp to the runtime behavior, after the does> call
        ]);
}

#[test]
fn test_normal_define() {
    // Defining a word and calling it, with the normal colon / semicolon words
    init_cpu().test_line(": fives 5 5 5 ; fives").expect_stack([5, 5, 5]).expect_empty_rstack();
}

#[test]
fn test_asm() {
    // Basic use of asm
    init_cpu().test_line("create execute $ jmp asm").expect_heap([
        ascii("execute"),
        ptr(heap(14)),
        skip(3),
        inst1("jmp")
    ]);

    // Asm with args
    init_cpu().test_line("45 $ push #asm").expect_heap([
        inst4("push", 45)
    ]);
}

#[test]
fn test_compile_mode_asm() {
    // Compile-mode asm
    init_cpu().test_line("] $ jmp asm").expect_heap([
        inst4("push", op("jmp")),
        inst4("call", ptr("compile_instruction")),
    ]);

    // Compile-mode asm with args
    init_cpu().test_line("] 45 $ xor #asm").expect_heap([
        inst4("push", 45),
        inst4("push", op("xor")),
        inst4("call", ptr("compile_instruction_arg"))
    ]);

    init_cpu().test_line(": foo 34 $ xor #asm ; immediate ] foo").expect_heap([
        // Foo's header
        ascii("foo"), ptr(heap(10)), ptr("compile_dict_start"),
        inst4("push", 34), // Push an arg
        inst4("push", op("xor")), // Push an opcode
        inst4("call", ptr("compile_instruction_arg")), // Compile that with an arg
        inst1("ret"), // Return from foo
        // Foo is now an immediate word, and when we call it in compile mode...
        inst4("xor", 34) // It compiles a xor 34
    ]);

    init_cpu().test_line("$ xor 3").expect_stack([9, 3]);

    init_cpu().test_line("$ blah 3").expect_empty_stack().expect_output("Invalid mnemonic: blah\n");

    init_cpu().test_line("] $ xor 3").expect_empty_stack().expect_heap([
        inst4("push", 9),
        inst4("push", 3)
    ]);

    init_cpu().test_line("] $ blah 3")
        .expect_empty_stack()
        .expect_output("Invalid mnemonic: blah\n")
        .expect_pointer("heap", "heap_start"); // It hits quit right after the error
}

#[test]
fn test_comma_compile() {
    // Comma compile a number
    init_cpu().test_line("1234 ,").expect_heap([num(1234)]);
}

#[test]
fn test_tick() {
    // Tick a word
    init_cpu().test_line("' print").expect_stack([SYMBOLS["print"]]);

    // Bracket-tick a word
    init_cpu().test_line("] ['] print")
        .expect_heap([inst4("push", ptr("print"))]);

    // Tick gibberish
    init_cpu().test_line("' bananas")
        .expect_empty_stack()
        .expect_empty_rstack()
        .expect_output("Not a word: bananas\n");

    // Bracket-tick gibberish
    init_cpu().test_line("] ['] penguin")
        .expect_empty_stack()
        .expect_empty_rstack()
        .expect_output("Not a word: penguin\n");

    // Tick a compile word
    init_cpu().test_line("' [").expect_stack([SYMBOLS["nova_open_bracket"]]);

    // Bracket-tick a compile word
    init_cpu().test_line("] ['] does>")
        .expect_heap([inst4("push", ptr("does_word"))]);
}

#[test]
fn test_pad() {
    // Fetch the pad address
    init_cpu().test_line("  pad  ").expect_stack([SYMBOLS["pad"]]);

    // Read a word to the pad
    init_cpu().test_line("word mango")
        .expect_output("")
        .expect_stack([SYMBOLS["pad"]])
        .expect_pad([ascii("mango")]);
}

#[test]
fn test_literal() {
    // Literal, compiles a push instruction
    init_cpu().test_line("1234 ] literal")
        .expect_empty_stack()
        .expect_heap([inst4("push", 1234)]);
}

#[test]
fn test_comments() {
    // Paren comments
    init_cpu().test_line("1 2 ( 3 4 5 ) 6").expect_stack([1, 2, 6]);

    // Nested paren comments
    init_cpu().test_line("1 2 ( ( 3 4 ) 5 6").expect_stack([1, 2]);

    // Compiled paren comments
    init_cpu().test_line("] 1 2 ( 3 4 5 ) 6").expect_heap([skip(12)]);

    // Compiled nested paren comments
    init_cpu().test_line("] 1 2 ( ( 3 4 ) 5 6").expect_heap([skip(8)]);

    // Backslash comments
    init_cpu().test_line("1 2 \\ 3 4").test_line("5 6").expect_stack([1, 2, 5, 6]);

    // Compiled backslash comments
    init_cpu().test_line("] 1 2 \\ 3 4").test_line("5 6").expect_heap([skip(16)]);
}

#[test]
fn test_parse_numbers() {
    // Parse numbers from words
    init_cpu().test_line("number 17").expect_stack([17, 1]);
    init_cpu().test_line("number blah").expect_stack([0]);
    init_cpu().test_line("number -23").expect_stack([-23 & 0xffffff, 1]);

    // Parse hex numbers from words
    init_cpu().test_line("hex number a4").expect_stack([164, 1]);
    init_cpu().test_line("hex number blah").expect_stack([0]);

    // Switch between hex and dec
    init_cpu().test_line("hex number a4 dec number 23").expect_stack([164, 1, 23, 1]);
    init_cpu().test_line("hex a4 dec 23").expect_stack([164, 23]);
}

#[test]
fn test_number_output() {
    // Output in hex and dec
    init_cpu().test_line("hex a4 . dec 23 .").expect_output("a423"); // Yeah, no separator
    init_cpu().test_line("hex a4 dec .").expect_output("164");
    init_cpu().test_line("dec 525 hex .").expect_output("20d");
    init_cpu().test_line("-15 .").expect_output("-15");
}

#[test]
fn test_compile_strings() {
    // Compiling strings to the heap
    init_cpu().test_line("s\" foo\"")
        .expect_stack([SYMBOLS["heap_start"]])
        .expect_cursor(7)
        .expect_heap([ascii("foo")]);

    // Compiling empty string
    init_cpu().test_line("s\" \"")
        .expect_stack([SYMBOLS["heap_start"]])
        .expect_heap([ascii("")]);

    // Unterminated string
    init_cpu().test_line("s\" foo")
        .expect_empty_stack()
        .expect_cursor(6)
        .expect_output("Unclosed string")
        .expect_pointer("heap", "heap_start");

    // Compile move squote
    init_cpu().test_line("] s\" blah\"")
        .expect_heap([
            inst4("jmpr", num(9)), // length of the jmpr itself + 'blah\0'
            ascii("blah"), // The actual string
            inst4("push", ptr(heap(4))) // Push the addr of the string
        ]);

    // Compile mode unterminated string
    init_cpu().test_line("] s\" foo")
        .expect_heap([])
        .expect_output("Unclosed string");
}

#[test]
fn test_output() {
    // Basic output
    init_cpu().test_line(".\" foo\"")
        .expect_empty_stack().expect_heap([])
        .expect_output("foo");

    // Compile output
    init_cpu().test_line("] .\" foo\"")
        .expect_heap([
            inst4("jmpr", 8),
            ascii("foo"),
            inst4("push", ptr(heap(4))),
            inst4("call", ptr("print"))
        ]);

    // Unterminated output
    init_cpu().test_line(".\" foo")
        .expect_empty_stack().expect_heap([])
        .expect_output("Unclosed string");

    // Compile output
    init_cpu().test_line("] .\" foo")
        .expect_heap([])
        .expect_output("Unclosed string");
}

#[test]
fn test_print() {
    init_cpu().test_line("s\" foo\" print")
        .expect_cursor(13)
        .expect_heap([ascii("foo")])
        .expect_output("foo");
}

#[test]
fn test_compare() {
    init_cpu().test_line("s\" foo\" s\" bar\" compare").expect_stack([0]);
    init_cpu().test_line("s\" foo\" s\" foo\" compare").expect_stack([1]);
    init_cpu().test_line("s\" foo\" ?dup compare").expect_stack([1]); // There's no simple dup...
    init_cpu().test_line("s\" foo\" s\" foo234\" compare").expect_stack([0]);
    init_cpu().test_line("s\" foo123\" s\" foo\" compare").expect_stack([0]);
}

#[test]
fn test_print_stack() {
    // Print the stack
    init_cpu().test_line("10 20 30 .s")
        .expect_stack([ 10, 20, 30 ])
        .expect_output("<< 10 20 30 >>");

    // Print the stack in hex
    init_cpu().test_line("10 20 30 hex .s")
        .expect_stack([ 10, 20, 30 ])
        .expect_output("<< a 14 1e >>");

    // Print nothing
    init_cpu().test_line(".s")
        .expect_empty_stack()
        .expect_output("<< >>");
}

#[test]
fn test_4th_rstack() {
    // pushr, peekr
    init_cpu().test_line("3 >r r@")
        .expect_stack([3])
        .expect_4th_rstack([num(3)]);

    // popr
    init_cpu().test_line("3 >r 5 r>")
        .expect_stack([5, 3])
        .expect_4th_rstack([]);

    // rpick
    init_cpu().test_line("10 20 30 >r >r >r 2 rpick")
        .expect_stack([30])
        .expect_4th_rstack([num(30), num(20), num(10)]);
}

#[test]
fn test_heap_ptr() {
    init_cpu().test_line("&heap").expect_stack([SYMBOLS["heap"]]);
    init_cpu().test_line("here").expect_stack([SYMBOLS["heap_start"]]);
}

#[test]
fn test_to_asm_resolve() {
    // To-asm
    init_cpu().test_line("$ brnz >asm")
        .expect_empty_stack()
        .expect_heap([inst4("brnz", 0)])
        .expect_4th_rstack([ptr(heap(1))]);

    // Resolve
    init_cpu().test_line("$ brnz >asm resolve")
        .expect_4th_rstack([])
        .expect_heap([inst4("brnz", 4)]); // brnz 12 ahead
}

#[test]
fn test_if() {
    // An 'if' implementation
    init_cpu().test_line(": if $ brz >asm ; immediate ] if")
        .expect_empty_stack()
        .expect_4th_rstack([ptr(heap(9 + 9 + 1))]) // Address of said brnz' arg
        .expect_heap([
            skip(9), // Skip if's header
            inst4("push", op("brz")),
            inst4("call", ptr("nova_asm_to")),
            inst1("ret"),
            inst4("brz", num(0)) // The unresolved brnz 'if' compiled
        ]);

        // If / then
        init_cpu()
            .test_line(": if $ brz >asm ; immediate")
            .test_line(": then resolve ; immediate")
            .test_line(": foo if 2 then ;")
            .test_line("1 foo 10 0 foo")
            .expect_stack([2, 10]);

        // If / else / then
        init_cpu()
            .test_line(": if $ brz >asm ; immediate")
            .test_line(": then resolve ; immediate")
            .test_line(": else r> $ jmpr >asm >r resolve ; immediate")
            .test_line(": foo if 2 else 3 then ;")
            .test_line("1 foo 10 0 foo")
            .expect_stack([2, 10, 3]);
}

#[test]
fn test_loops() {
    // Begin / until loops
    init_cpu()
        .test_line(": begin here >r ; immediate") // Begin just marks a point in the program we'll brnz back to
        // Here's the fun part.
        // Pull the address stored by 'begin' off the rstack and subtract `here` from it
        // Then compile a brz to that address
        .test_line(": until r> here - $ brz #asm ; immediate")
        // This ought to loop from 5..0, leaving each one on the stack
        .test_line(": foo 5 begin dup 1 - dup not until ; foo")
        .expect_stack([5, 4, 3, 2, 1, 0]);

        // do / loop counted loops
        init_cpu()
            .test_line("create 1+ 1 $ add #asm ] ;")
            .test_line(": do postpone swap postpone >r postpone >r here >r ; immediate")
            .test_line(": _loop_test r> 1+ dup r@ < swap >r ;") // pull off and inc the cntr, dup, peek at the limit, compare them, put the new cntr back
            .test_line(": unloop r> r> pop pop ;")
            .test_line(": loop postpone _loop_test r> here - $ brnz #asm postpone unloop ; immediate")
            .test_line(": foo 3 0 do 33 loop ; foo")
            .expect_stack([33, 33, 33]);
}

#[test]
fn test_quit() {
    // Testing quit as called by an error
    init_cpu().test_line("2 3 : foo nooope ; 7")
        .expect_heap([skip(10)]) // It does the header but that's it
        .expect_output("Not a word: nooope\n") // Spits out an error message
        .expect_pointer("handleword_hook", "immediate_handleword") // Back in immediate mode
        .expect_empty_stack(); // Clobbers the stack

        // Testing quit as called manually
        init_cpu().test_line(": low 3 quit 65 emit ;")
            .test_line(": med 2 low 66 emit ;")
            .test_line(": high 1 med 67 emit ;")
            .test_line("high")
            .expect_output("") // This isn't an error, we just quit
            .expect_empty_stack(); // We quit partway through 'low', so skip all the frames above that
}

#[test]
fn test_lambdas() {
    // Testing immediate-mode lambdas
    init_cpu().test_line("{ 3 5 }")
        .expect_output("")
        .expect_pointer("handleword_hook", "immediate_handleword") // Back in immediate mode
        .expect_stack([SYMBOLS["heap_start"]]) // Leaves the address of the lambda on the stack
        .expect_heap([]) // It does not move the heap, but things are stored after the heap ptr, even though it hasn't moved
        .expect_memory("heap_start",[
            inst4("push", 3),
            inst4("push", 5),
            inst1("ret")
        ]);

    init_cpu().test_line("{ 3 5 } execute").expect_stack([3, 5]); // Runs the anonymous fn

    // Compile-mode lambda, non-nested
    init_cpu().test_line(": foo 1 { 2 } ; foo")
        .expect_output("")
        .expect_heap([
            skip(10),
            inst4("push", 1),
            inst4("jmpr", 4+4+1), // jmpr, push, ret
            inst4("push", 2),
            inst1("ret"),
            inst4("push", ptr(heap(10+4+4))), // header, push(1), jmpr
            inst1("ret")
        ])
        .expect_stack([1, u32::from(SYMBOLS["heap_start"]) + 10 + 4 + 4]);

        // Compile-mode lambda, nested
        init_cpu().test_line(": foo 1 { 2 { 3 } } ; foo")
            .expect_output("")
            .expect_heap([
                skip(10),
                inst4("push", 1),
                inst4("jmpr", 4*4 + 1 + 4 + 1), // jmpr, push(2), jmpr, push(3), ret, push(inner-lambda), ret
                inst4("push", 2),
                inst4("jmpr", 4+4+1), // inner lambda: jmpr, push, ret
                inst4("push", 3),
                inst1("ret"),
                inst4("push", ptr(heap(10 + 4 * 4))), // push the inner-lambda addr
                inst1("ret"),
                inst4("push", ptr(heap(10+4+4))),
                inst1("ret")
            ])
            .expect_stack([1, u32::from(SYMBOLS["heap_start"]) + 10 + 4 + 4])
            .expect_var("lambda_nesting_level", 0);

        // Executing nested compile-mode lambdas
        init_cpu().test_line(": foo 1 { 2 { 3 } } ; foo execute execute").expect_stack([1, 2, 3]);
}

#[test]
fn test_graham_accumulator() {
    init_cpu()
        .test_line(": accum create 0 , does> dup >r @ + dup r> ! ;")
        .test_line("accum foo 1 foo 2 foo 3 foo")
        .expect_output("")
        .expect_stack([1, 3, 6]);
}

#[test]
fn test_single_opcode_words() {
    // If it didn't recognize any of these then it would error
    init_cpu().test_line(": test + - / * % ^ & | not < > = @ ! c@ c! pop dup swap pick rot ;")
        .expect_output("");
}

#[test]
fn print_novaforth_stats() {
    let heap: u32 = SYMBOLS["heap"].into();
    let heap_start: u32 = SYMBOLS["heap_start"].into();
    let data_start: u32 = SYMBOLS["data_start"].into();

    println!("Bytes available: {}", 131072 - heap_start);
    println!("Text size: {}", data_start - 0x400);
    println!("Including dictionaries: {}", heap - 0x400);
    println!("Remaining in 4k: {}", 4096 - (heap - 0x400));
}