.asm

.function $entrypoint
    val.w 123
    val.w $a
    st
    val.w 312
    val.w $b
    st
    br $compute

.res $a      word
.res $b      word
.res $result word

.function $compute:
    val.w $a
    ld
    val.w $b
    ld
    sub
    cmp.zs
    br.if eq 
$neq:
    val.w 127
    br $end
$eq:
    val.w 63
$end:
    val.w $result
    st
    hlt
    
.endasm