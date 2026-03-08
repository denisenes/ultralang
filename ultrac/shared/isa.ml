module Instruction = struct

  type width =
  | W8
  | W16
  [@@deriving enum]

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

  let arity: opcode -> int = function
    | HALT | CONST_ZERO | DUP | SWAP | DROP | BIN_ADD | BIN_SUB | BIN_MUL | BIN_DIV | BIN_REM 
    | BIN_OR | BIN_AND | BIN_XOR | BIN_SHR | BIN_SHL | BIN_CMP_U | BIN_CMP_S | UN_NEG | UN_INC | UN_DEC 
    | UN_CMP_ZERO_U | UN_CMP_ZERO_S | UN_LOW | UN_HIGH | LOAD_IP | BR_DYN | RET -> 0
    | CONSTW | CONST | LOAD | LOAD_DYN_OFFS | LOAD_LOC | STORE | STORE_DYN_OFFS | STORE_LOC | BR | BR_IF_DYN 
    | CALL_DYN | LALLOC | LFREE | INT -> 1
    | LOAD_OFFS | STORE_OFFS | BR_IF | CALL | INT_SET_HANDLER -> 2

  let width (opcode: opcode) (n: int): width = match opcode, n with
  | CONSTW, 0 -> W16
  | CONST, 0 -> W8
  | LOAD, 0 -> W8
  | LOAD_OFFS, 0 -> W8
  | LOAD_OFFS, 1 -> W16
  | LOAD_DYN_OFFS, 0 -> W8
  | LOAD_LOC, 0 -> W16
  | STORE, 0 -> W8
  | STORE_OFFS, 0 -> W8
  | STORE_OFFS, 1 -> W16
  | STORE_DYN_OFFS, 0 -> W8
  | STORE_LOC, 0 -> W16
  | BR, 0 -> W16
  | BR_IF, 0 -> W8
  | BR_IF, 1 -> W16
  | BR_IF_DYN, 0 -> W8
  | CALL, 0 -> W16
  | CALL, 1 -> W8
  | CALL_DYN, 0 -> W8
  | LALLOC, 0 -> W16
  | LFREE, 0 -> W16
  | INT, 0 -> W8
  | INT_SET_HANDLER, 0 -> W8
  | INT_SET_HANDLER, 1 -> W16
  | _ -> Printf.sprintf "[Opcode: %d, operand: %d] incorrect combination" (opcode_to_enum opcode) n
      |> Utils.shouldNotReachHere

  let from_string: string -> opcode = function
  | "hlt"     -> HALT
  | "val.z"   -> CONST_ZERO
  | "val.w"   -> CONSTW
  | "val"     -> CONST
  | "dup"     -> DUP
  | "swp"     -> SWAP
  | "drp"     -> DROP
  | "add"     -> BIN_ADD
  | "sub"     -> BIN_SUB
  | "mul"     -> BIN_MUL
  | "div"     -> BIN_DIV
  | "rem"     -> BIN_REM
  | "and"     -> BIN_AND
  | "or"      -> BIN_OR
  | "xor"     -> BIN_XOR
  | "shr"     -> BIN_SHR
  | "shl"     -> BIN_SHL
  | "cmp.u"   -> BIN_CMP_U
  | "cmp.s"   -> BIN_CMP_S
  | "neg"     -> UN_NEG
  | "inc"     -> UN_INC
  | "dec"     -> UN_DEC
  | "cmp.zu"  -> UN_CMP_ZERO_U
  | "cmp.zs"  -> UN_CMP_ZERO_S
  | "lo"      -> UN_LOW
  | "hi"      -> UN_HIGH
  | "ld.ip"   -> LOAD_IP
  | "ld"      -> LOAD
  | "ld.o"    -> LOAD_OFFS
  | "ld.d"    -> LOAD_DYN_OFFS
  | "ld.l"    -> LOAD_LOC
  | "st"      -> STORE
  | "st.o"    -> STORE_OFFS
  | "st.d"    -> STORE_DYN_OFFS
  | "st.l"    -> STORE_LOC
  | "br"      -> BR
  | "br.d"    -> BR_DYN
  | "br.if"   -> BR_IF
  | "br.id"   -> BR_IF_DYN
  | "call"    -> CALL
  | "call.d"  -> CALL_DYN
  | "ret"     -> RET
  | "lalloc"  -> LALLOC
  | "lfree"   -> LFREE
  | "int"     -> INT
  | "int.h"   -> INT_SET_HANDLER
  | _         -> Utils.shouldNotReachHere "Unknown opcode"

  type operand = 
    | Label of string 
    | W8    of int 
    | W16   of int
  
  type t = {
    uop:   opcode;
    opnds: operand list;
  }

  let constr0 (op: opcode): t =
    { uop = op; opnds = [] }
  let constr1 (op: opcode) (arg1: operand) =
    { uop = op; opnds = [arg1] }
  let constr2 (op: opcode) (arg1: operand) (arg2: operand) =
    { uop = op; opnds = [arg1; arg2] }
  let constr3 (op: opcode) (arg1: operand) (arg2: operand) (arg3: operand) =
    { uop = op; opnds = [arg1; arg2; arg3] }

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

  let meta (v: Meta.t): elem = Meta v

  let instr (v: Instruction.t): elem = Instruction v

  type t = {
    kind: kind;
    start_addr: int option;
    size: int option;
    seq: elem list;
    bin: bytes option;
  }

end