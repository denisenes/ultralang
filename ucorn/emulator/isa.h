#ifndef H_ISA
#define H_ISA

enum Opcode {
    HALT             = 0x00, // hlt     0
    CONST_ZERO       = 0x01, // val.z   0
    CONSTW           = 0x02, // val.w   1:16
    CONST            = 0x03, // val     1:8
    DUP              = 0x04, // dup     0
    SWAP             = 0x05, // swp     0
    DROP             = 0x06, // drp     0
    BIN_ADD          = 0x07, // add     0
    BIN_SUB          = 0x08, // sub     0
    BIN_MUL          = 0x09, // mul     0
    BIN_DIV          = 0x0A, // div     0
    BIN_REM          = 0x0B, // rem     0
    BIN_OR           = 0x0C, // or      0
    BIN_AND          = 0x0D, // and     0
    BIN_XOR          = 0x0E, // xor     0
    BIN_SHR          = 0x0F, // shr     0
    BIN_SHL          = 0x10, // shl     0
    BIN_CMP_U        = 0x11, // cmp.u   0
    BIN_CMP_S        = 0x12, // cmp.s   0
    UN_NEG           = 0x13, // neg     0
    UN_INC           = 0x14, // inc     0
    UN_DEC           = 0x15, // dec     0
    UN_CMP_ZERO_U    = 0x16, // cmp.zu  0
    UN_CMP_ZERO_S    = 0x17, // cmp.zs  0
    UN_LOW           = 0x18, // lo      0
    UN_HIGH          = 0x19, // hi      0
    LOAD_IP          = 0x1A, // ld.ip   0
    LOAD             = 0x1B, // ld      1:8
    LOAD_OFFS        = 0x1C, // ld      2:8,16
    LOAD_DYN_OFFS    = 0x1D, // ld.d    1:8
    LOAD_LOC         = 0x1E, // ld.l    1:16
    STORE            = 0x1F, // st      1:8
    STORE_OFFS       = 0x20, // st      2:6,16
    STORE_DYN_OFFS   = 0x21, // st.d    1:8
    STORE_LOC        = 0x22, // st.l    1:16
    BR               = 0x23, // br      1:16
    BR_DYN           = 0x24, // br.d    0
    BR_IF            = 0x25, // br.if   2:8,16
    BR_IF_DYN        = 0x26, // br.id   1:8
    CALL             = 0x27, // call    2:16,8
    CALL_DYN         = 0x28, // call    1:8
    RET              = 0x29, // ret     0
    LALLOC           = 0x2A, // lalloc  1:16
    LFREE            = 0x2B, // lfree   1:16
    INT              = 0x2C, // int     1:8
    INT_SET_HANDLER  = 0x2D  // int.h   2:8,16
};

#endif