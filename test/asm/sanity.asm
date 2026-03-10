.asm SanityTest

.function $entrypoint:
    val.w 123
    val.w $a
    st WORD
    val.w 312
    val.w $b
    st WORD
    br $compute

.res $a      word
.res $b      word
.res $result word

.function $compute:
    val.w $a
    ld WORD
    val.w $b
    ld WORD 
    sub
    cmp.zs
    br.if eq $eq
$neq:
    val.w 127
    br $end
$eq:
    val.w 63
$epilogue:
    val.w $result
    st WORD
    hlt
    
.endasm
