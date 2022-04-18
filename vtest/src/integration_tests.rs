use std::collections::BTreeMap;
use vasm::assemble_snippet;
use vcore::memory::{Memory, PeekPokeExt};
use vcore::CPU;

fn cpu_test<'a, T: IntoIterator<Item = &'a str>>(code: T) -> CPU {
    let bin = assemble_snippet(code).unwrap();
    let mut cpu = vcore::cpu::CPU::new(Memory::with_program(bin));
    cpu.run_to_halt();
    cpu
}

fn test_stack<'a, T: IntoIterator<Item = &'a str>>(code: T, expected_stack: Vec<i32>) {
    let cpu = cpu_test(code);
    assert_eq!(cpu.get_stack(), expected_stack);
}

fn test_rstack<'a, T: IntoIterator<Item = &'a str>>(
    code: T,
    expected_stack: Vec<i32>,
    expected_rstack: Vec<i32>,
) {
    let cpu = cpu_test(code);
    assert_eq!(cpu.get_stack(), expected_stack);
    assert_eq!(cpu.get_call(), expected_rstack);
}

fn test_mem<'a, T: IntoIterator<Item = &'a str>>(code: T, expected_memory: BTreeMap<u32, u8>) {
    let cpu = cpu_test(code);
    for (addr, byte) in expected_memory {
        assert_eq!(cpu.peek8(addr), byte)
    }
}

#[test]
fn test_arithmetic() {
    // Basic arithmetic
    test_stack(
        ".org 0x400
              push 2
              add 3
              hlt"
        .lines(),
        vec![5],
    );

    // Multibyte values
    test_stack(
        ".org 0x400
        push 7
        mul 1000
        hlt"
        .lines(),
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
        .lines(),
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
        .lines(),
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
        .lines(),
        vec![10, 7],
        vec![20],
    );

    test_rstack(
        ".org 0x400
        pushr 5
        call blah
        blah: pushr 3
        hlt"
        .lines(),
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
        .lines(),
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
        .lines(),
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
        .lines(),
        vec![0, 1, 1, 0],
    );

    test_stack(
        ".org 1024
        not 10
        not 0
        hlt"
        .lines(),
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
        .lines(),
        vec![10, 20, 10],
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
        .lines(),
        [(201, 10), (203, 0x56), (204, 0x34), (205, 0x12)].into(),
    )
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
            .lines(),
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
        .lines(),
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
        .lines(),
        vec![10, 1021, 265],
    )
}
