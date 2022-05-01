.org 0x400
screen: .equ 0x10000
reg: .equ 16

;;;;; Start
    push on_key
    setiv 5
    setint 1
    call clear_screen
    call set_video
    push msg
    push screen
    call print
wfi_loop: hlt
    jmpr @wfi_loop
;;;;;

set_video:
    push 30
    storew reg + 10
    push 40
    storew reg + 13
    ret

clear_screen:
    push screen
    #while
    dup
    lt screen + 40 * 30
    #do
    dup
    swap 0
    store

    dup
    add 40 * 30
    swap 0b10010010
    store

    add 1
    #end
    pop
    ret

print: ; ( msg addr -- )
    pushr
    #while
    dup
    load
    #do
    dup
    load
    peekr
    store
    popr
    add 1
    pushr
    add 1
    #end
    pop
    popr
    pop
    ret

on_key:
    setint 1 ; this can be reentrant, doesn't hurt anything
    call is_press ; check for press
    #if
        ; if is alpha, look up in map and print
        dup
        call is_alpha
        #if
        call to_alpha_char
        call putc
        #else
        pop
        #end
    #else
    ; TODO:
    ;   if is punc, look up in punc map and print
    ;   if shift, set shift flag
    ;   if return inc cursor
    ;   if backspace, clear cursor and dec
    ; else release:
    ;   if shift, clear shift flag
    pop
    #end
    ret

is_press: ; ( event -- key bool )
    dup
    and 0xff
    swap
    and 0xff00
    ret

is_alpha: ; ( key -- bool )
    dup
    gt 0x03
    swap
    lt 0x27
    and
    ret

is_punc: ; ( key -- bool )
    dup
    gt 0x2b
    swap
    lt 0x39
    and
    ret

to_alpha_char: ; ( key -- ch )
    sub 0x04
    add alpha_table
    load
    ret

putc: ; ( ch -- ) (also modifies cursor)
    loadw cursor
    dup
    pushr
    store
    popr
    add 1
    storew cursor
    ret

cursor: .db screen + 40
msg: .db "Type something:\0"
alpha_table: .db "abcdefghijklmnopqrstuvwxyz1234567890"