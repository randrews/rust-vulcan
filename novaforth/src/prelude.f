: if $ brz >asm ; immediate
: then resolve ; immediate
: else r> $ jmpr >asm >r resolve ; immediate
: variable create 0 , does> ;
: unloop r> r> drop drop ;
: leave r> drop r@ >r ;
: begin here >r ; immediate
: until r> here - $ brz #asm ; immediate

: xor [ $ xor asm ] ;
: arshift [ $ arshift asm ] ;
: rshift [ $ rshift asm ] ;
: lshift [ $ lshift asm ] ;
: u> [ $ gt asm ] ;
: u< [ $ lt asm ] ;
: rdrop r> drop ;
: over 1 pick ;
: nip swap drop ;
: -rot rot rot ;
: tuck dup -rot ;
: emit 2 c! ;
: space 32 emit ;
: cr 10 emit ;
: +! dup @ rot + swap ! ;
: dup2 1 pick 1 pick ;
: allot &heap +! here ;
: negate -1 xor 1+ ;
: free negate &heap +! here ;
: c+! dup c@ rot + swap c! ;
: false 0 ;
: true 1 ;
: ror dup 1 rshift swap 23 lshift or ;
: rol dup 23 rshift swap 1 lshift or ;
: abs dup 0 < if negate then ;
: spaces 0 do space loop ;
