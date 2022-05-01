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
        call is_alpha
        brnz @putc
        call is_punc
        brnz @putc
        pop
        ret
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

is_ret: ; ( key -- 1 ) or ( key -- key 0 )
    dup
    sub 0x28
    brnz @+3
    pop
    ret 1
    ret 0

is_alpha: ; ( key -- ch 1 ) or ( key -- key 0 )
    dup
    dup
    gt 0x03
    swap
    lt 0x28
    and
    #if
    sub 0x04
    add alpha_table
    load
    ret 1
    #end
    ret 0

is_punc: ; ( key -- ch 1 ) or (key -- key 0 )
    dup
    dup
    gt 0x2b
    swap
    lt 0x39
    and
    #if
    sub 0x2c
    add punc_table
    load
    ret 1
    #end
    ret 0

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
punc_table: .db " -=[]\\?;'`,./"