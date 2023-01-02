: if $ brz >asm ; immediate
: then resolve ; immediate
: else r> $ jmpr >asm >r resolve ; immediate
: variable create 0 , does> ;
: arshift [ $ arshift asm ] ;
: lshift [ $ lshift asm ] ;
: rshift [ $ rshift asm ] ;
: u> [ $ gt asm ] ;
: u< [ $ lt asm ] ;
: rdrop r> pop ;
: over 1 pick ;
: nip swap pop ;
: -rot rot rot ;
: tuck dup -rot ;
: space 32 emit ;
: cr 13 emit 10 emit ;
: +! dup @ rot + swap ! ;
: 2dup 1 pick 1 pick ;
: allot here swap &heap +! ;
: negate -1 ^ 1 + ;
: free negate &heap +! here ;
: c+! dup c@ rot + swap c! ;
: ror dup 1 rshift swap 23 lshift | ;
: rol dup 23 rshift swap 1 lshift | ;
: abs dup 0 < if negate then ;
: begin here >r ; immediate
: until r> here - $ brz #asm ; immediate