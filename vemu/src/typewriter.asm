.org 0x400
screen: .equ 0x10000
reg: .equ 16

lshift: .equ 0xe1
rshift: .equ 0xe5
enter: .equ 0x28
backspace: .equ 0x2a

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
        call is_ret
        brnz @newline
        call is_back
        brnz @handle_backspace
        call is_printable
        brnz @putc
        call is_shift
        brnz @set_shift
    ;   if backspace, clear cursor and dec
        pop
        ret
    #else
        call is_shift
        brnz @clear_shift
        pop
        ret
    #end
    ret

newline:
    loadw cursor
    sub screen
    add 40
    dup
    gt 40 * 30 - 1
    #if
    pop
    push 40
    #else
    dup
    mod 40
    sub
    #end
    add screen
    storew cursor
    ret

handle_backspace:
    loadw cursor
    sub screen
    dup
    mod 40
    #if
    sub 1
    add screen
    dup
    swap 32 ; a space
    store
    storew cursor
    #else
    pop
    #end
    ret


set_shift:
    push shift_table
    storew current_table
    ret

clear_shift:
    push default_table
    storew current_table
    ret

is_press: ; ( event -- key bool )
    dup
    and 0xff
    swap
    and 0xff00
    ret

is_ret:
    push enter
    jmp is_key

is_back:
    push backspace
    jmp is_key

is_shift: ; ( key -- 1 ) or ( key -- key 0 )
    push lshift
    call is_key
    brz @+2
    ret 1
    push rshift
    jmp is_key

is_key: ; ( key1 key2 -- 1 ) or ( key1 key2 -- key1 0 )
    swap
    dup
    pushr
    sub
    #unless ; they're equal
    popr
    pop
    ret 1
    #else ; they're not
    popr
    ret 0
    #end

is_printable: ; ( key -- ch 1 ) or ( key -- key 0 )
    dup
    dup
    gt 0x03
    swap
    lt 0x39
    and
    #if
    sub 0x04
    loadw current_table
    add
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

default_table: .db "abcdefghijklmnopqrstuvwxyz1234567890???? -=[]\\?;'`,./"
shift_table: .db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()???? _+{}|?:\"~<>?"

cursor: .db screen + 40
msg: .db "Type something:\0"
current_table: .db default_table
