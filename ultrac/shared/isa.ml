module Instruction = struct

  (** ! Keep consistent with
   *  ${PROJECT_DIR}/ucorn/emulator/isa.h !
   *)
  type opcode =
    | HALT            (* hlt     0 *)
    | CONST_ZERO      (* val.z   0 *)
    | CONSTW          (* val.w   1:16 *)
    | CONST           (* val     1:8 *)
    | DUP             (* dup     0 *)
    | SWAP            (* swp     0 *)
    | DROP            (* drp     0 *)
    | BIN_ADD         (* add     0 *)
    | BIN_SUB         (* sub     0 *)
    | BIN_MUL         (* mul     0 *)
    | BIN_DIV         (* div     0 *)
    | BIN_REM         (* rem     0 *)
    | BIN_OR          (* or      0 *)
    | BIN_AND         (* and     0 *)
    | BIN_XOR         (* xor     0 *)
    | BIN_SHR         (* shr     0 *)
    | BIN_SHL         (* shl     0 *)
    | BIN_CMP_U       (* cmp.u   0 *)
    | BIN_CMP_S       (* cmp.s   0 *)
    | UN_NEG          (* neg     0 *)
    | UN_INC          (* inc     0 *)
    | UN_DEC          (* dec     0 *)
    | UN_CMP_ZERO_U   (* cmp.zu  0 *)
    | UN_CMP_ZERO_S   (* cmp.zs  0 *)
    | UN_LOW          (* lo      0 *)
    | UN_HIGH         (* hi      0 *)
    | LOAD_IP         (* ld.ip   0 *)
    | LOAD            (* ld      1:8 *)
    | LOAD_OFFS       (* ld      2:8,16 *)
    | LOAD_DYN_OFFS   (* ld.d    1:8 *)
    | LOAD_LOC        (* ld.l    1:16 *)
    | STORE           (* st      1:8 *)
    | STORE_OFFS      (* st      2:6,16 *)
    | STORE_DYN_OFFS  (* st.d    1:8 *)
    | STORE_LOC       (* st.l    1:16 *)
    | BR              (* br      1:16 *)
    | BR_DYN          (* br.d    0 *)
    | BR_IF           (* br.if   2:8,16 *)
    | BR_IF_DYN       (* br.id   1:8 *)
    | CALL            (* call    2:16,8 *)
    | CALL_DYN        (* call    1:8 *)
    | RET             (* ret     0 *)
    | LALLOC          (* lalloc  1:16 *)
    | LFREE           (* lfree   1:16 *)
    | INT             (* int     1:8 *)
    | INT_SET_HANDLER (* int.h   2:8,16 *)
  [@@deriving enum]

  type operand = 
    | Label of string 
    | W8    of int 
    | W16   of int
  
  type t = {
    uop:   opcode;
    opnds: operand list;
  }

end


module Meta = struct
  type t = Label of string
end


module Block = struct

  type kind =
    | Reserved
    | Function

  type elem = 
    | Meta of Meta.t
    | Instruction of Instruction.t

  type t = {
    kind: kind;
    start_addr: int option;
    size: int option;
    seq: elem list;
    bin: bytes option;
  }

end