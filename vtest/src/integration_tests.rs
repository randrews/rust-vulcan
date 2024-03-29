use std::collections::btree_map::BTreeMap;
use vasm_core::assemble_snippet;
use vcore::memory::{Memory, PeekPokeExt};
use vcore::word::Word;
use vcore::CPU;

fn cpu_test<T: IntoIterator<Item = String>>(code: T) -> CPU {
    let bin = assemble_snippet(code).unwrap();
    let mut cpu = vcore::cpu::CPU::new(Memory::with_program(bin));
    cpu.run_to_halt();
    cpu
}

fn test_stack<T: IntoIterator<Item = String>>(code: T, expected_stack: Vec<i32>) {
    let cpu = cpu_test(code);
    assert_eq!(cpu.get_stack(), expected_stack);
}

fn test_rstack<T: IntoIterator<Item = String>>(
    code: T,
    expected_stack: Vec<i32>,
    expected_rstack: Vec<i32>,
) {
    let cpu = cpu_test(code);
    assert_eq!(cpu.get_stack(), expected_stack);
    assert_eq!(cpu.get_call(), expected_rstack);
}

fn test_mem<T: IntoIterator<Item = String>>(
    code: T,
    expected_memory: BTreeMap<u32, u8>,
) -> CPU {
    let cpu = cpu_test(code);
    for (addr, byte) in expected_memory {
        assert_eq!(cpu.peek8(addr), byte)
    }
    cpu
}

#[test]
fn test_arithmetic() {
    // Basic arithmetic
    test_stack(
        ".org 0x400
              push 2
              add 3
              hlt"
        .lines().map(String::from),
        vec![5],
    );

    // Multibyte values
    test_stack(
        ".org 0x400
        push 7
        mul 1000
        hlt"
        .lines().map(String::from),
        vec![7000],
    );
}

#[test]
fn test_call_stack() {
    test_stack(
        ".org 0x400
        nop 3
        call blah
        hlt
        blah: mul 2
        ret"
        .lines().map(String::from),
        vec![6],
    );

    test_rstack(
        ".org 0x400
        push 10
        push 4
        push 3
        pushr
        pushr
        hlt"
        .lines().map(String::from),
        vec![10],
        vec![3, 4],
    );

    test_rstack(
        ".org 0x400
        push 10
        pushr 20
        pushr 4
        push 3
        popr
        add
        hlt"
        .lines().map(String::from),
        vec![10, 7],
        vec![20],
    );

    test_rstack(
        ".org 0x400
        pushr 5
        call blah
        blah: pushr 3
        hlt"
        .lines().map(String::from),
        vec![],
        vec![5, 0x406, 3],
    );

    test_rstack(
        ".org 0x400
        push 5
        pushr 10
        pushr 20
        peekr
        popr
        popr
        hlt"
        .lines().map(String::from),
        vec![5, 20, 20, 10],
        vec![],
    )
}

#[test]
fn test_comparison() {
    test_stack(
        ".org 0x400
        push 10
        gt 20
        push 20
        gt 5
        push 10
        lt 20
        push 10
        lt 5
        hlt"
        .lines().map(String::from),
        vec![0, 1, 1, 0],
    );

    test_stack(
        ".org 1024
        push 10
        mul 0xffffff
        agt 20
        push 20
        agt -5
        push -10
        alt 20
        push 10
        alt -5
        hlt"
        .lines().map(String::from),
        vec![0, 1, 1, 0],
    );

    test_stack(
        ".org 1024
        not 10
        not 0
        hlt"
        .lines().map(String::from),
        vec![0, 1],
    )
}

#[test]
fn test_pick() {
    test_stack(
        ".org 1024
        nop 10
        nop 20
        pick 1
        hlt"
        .lines().map(String::from),
        vec![10, 20, 10],
    );

    test_stack(
        ".org 1024
        nop 10
        nop 20
        pick 2
        pick 5
        hlt"
        .lines().map(String::from),
        vec![10, 20, 0, 0],
    )
}

#[test]
fn test_store() {
    test_mem(
        ".org 0x400
        push 10
        store 201
        push 0x123456
        storew 203
        hlt"
        .lines().map(String::from),
        [(201, 10), (203, 0x56), (204, 0x34), (205, 0x12)].into(),
    );
}

#[test]
fn test_load() {
    test_stack(
        ".org 0x400
        load 0x501
        loadw 0x500
        hlt
        .org 0x500
        .db 0x123456"
            .lines().map(String::from),
        vec![0x34, 0x123456],
    )
}

#[test]
fn test_rotate() {
    test_stack(
        ".org 0x400
        push 10
        push 100
        push 200
        push 300
        rot
        hlt"
        .lines().map(String::from),
        vec![10, 200, 300, 100],
    )
}

#[test]
fn test_sdp() {
    test_stack(
        ".org 0x400
        push 10
        pushr 20
        sdp
        hlt"
        .lines().map(String::from),
        vec![10, 1021, 265],
    )
}

#[test]
fn test_underflow() {
    test_mem(
        ".org 0x400
        push 100
        store 10
        push onunder
        setiv 1
        add 3
        hlt
        onunder: push 200
        store 10
        hlt"
        .lines().map(String::from),
        [(10, 200)].into(),
    );

    test_mem(
        ".org 0x400
        push 100
        store 10
        push onrunder
        setiv 2
        peekr
        hlt
        onrunder: push 200
        store 10
        hlt"
        .lines().map(String::from),
        [(10, 200)].into(),
    );

    test_mem(
        ".org 0x400
        push 100
        store 10
        push onunder
        setiv 1
        div 0 ; This div should be an underflow, not a div 0
        hlt
        onunder: push 200
        store 10
        hlt"
        .lines().map(String::from),
        [(10, 200)].into(),
    );
}

#[test]
fn test_div_zero() {
    test_mem(
        ".org 0x400
        push 100
        store 10
        push ondiv0
        setiv 0
        push 5
        div 0
        hlt
        ondiv0: push 200
        store 10
        hlt"
        .lines().map(String::from),
        [(10, 200)].into(),
    );

    test_mem(
        ".org 0x400
        push 100
        store 10
        push ondiv0
        setiv 0
        push 5
        mod 0 ; Mods can raise this too
        hlt
        ondiv0: push 200
        store 10
        hlt"
        .lines().map(String::from),
        [(10, 200)].into(),
    );
}

#[test]
fn test_overflow() {
    let cpu = test_mem(
        ".org 0x400
        push onover
        setiv 3 ; set the overflow handler
        push 25
        setsdp 10 ; new stack of five cells
        pushr 0
        dup 0 ; spare room for handler; 2 cells left
        push 1
        push 2 ; fine so far
        .org 0x4ff ; bunch of nops and then...
        push 3 ; boom!
        store ; never happens
        onover: hlt"
            .lines().map(String::from),
        [(16, 1), (19, 2)].into(), // expect the two successful pushes to be still there
    );

    // Data stack now contains the collided pointers
    assert_eq!(vec![Word::from(22), Word::from(22)], cpu.get_stack());

    // Call stack contains the address of the offending instruction
    assert_eq!(vec![Word::from(0x4ff)], cpu.get_call());

    // Stack is now the previous bottom values, minus three cells:
    assert_eq!((22.into(), 16.into()), cpu.sdp());
}

#[test]
fn test_simple_overflow() {
    let cpu = test_mem(
        ".org 0x400
        push onover
        setiv 3 ; set the overflow handler
        push 25
        setsdp 10 ; new stack of five cells
        pushr 0
        dup 0 ; spare room for handler; 2 cells left
        push 1
        push 2 ; fine so far
        .org 0x4ff ; bunch of nops and then...
        add 5 ; full stack + argument!
        store ; never happens
        onover: hlt"
            .lines().map(String::from),
        [(16, 1), (19, 2)].into(), // expect the two successful pushes to be still there
    );

    // Data stack now contains the collided pointers
    assert_eq!(vec![Word::from(22), Word::from(22)], cpu.get_stack());

    // Call stack contains the address of the offending instruction
    assert_eq!(vec![Word::from(0x4ff)], cpu.get_call());

    // Stack is now the previous bottom values, minus three cells:
    assert_eq!((22.into(), 16.into()), cpu.sdp());
}

#[test]
fn test_overflow_promotion() {
    let cpu = test_mem(
        ".org 0x400
        push onover
        setiv 3 ; set the overflow handler
        push 25
        setsdp 10 ; new stack of five cells
        pushr 0
        dup 0 ; spare room for handler; 2 cells left
        push 1
        push 0 ; fine so far
        .org 0x4ff ; bunch of nops and then...
        div ; This is a div 0, but there's no room to handle it, so it's promoted to overflow
        store ; never happens
        onover: hlt"
            .lines().map(String::from),
        [(16, 1), (19, 0)].into(), // expect the two successful pushes to be still there
    );

    // Data stack now contains the collided pointers
    assert_eq!(vec![Word::from(22), Word::from(22)], cpu.get_stack());

    // Call stack contains the address of the offending instruction
    assert_eq!(vec![Word::from(0x4ff)], cpu.get_call());

    // Stack is now the previous bottom values, minus three cells:
    assert_eq!((22.into(), 16.into()), cpu.sdp());
}

#[test]
fn test_invalid_opcode() {
    let cpu = test_mem(
        ".org 0x400
        push onop
        setiv 4 ; set the overflow handler
        push 1
        push 2 ; fine so far
        .org 0x4ff ; bunch of nops and then...
        .db 0xf6 ; this isn't an instruction!
        store ; never happens
        onop: hlt"
            .lines().map(String::from),
        [(256, 1), (259, 2)].into(), // expect the two successful pushes to be still there
    );

    // Call stack contains the address of the offending instruction
    assert_eq!(vec![Word::from(0x4ff)], cpu.get_call());
}
