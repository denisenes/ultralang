let value_size = 8

type int_register = 
  RSP | 
  RBP |
  RAX | EAX | AX | AL |
  RBX | EBX | BX | BL |
  RCX | ECX | CX | CL |
  RDX | EDX | DX | DL |
  RSI | ESI |
  RDI | EDI

type address = Addr of int

type memory_ptr = 
  FromRegSubOff of int_register * int | (* [RSP - off] *)
  FromRegAddOff of int_register * int | (* [RSP + off] *)
  FromReg       of int_register       | (* [RSP] *)
  FromAddr      of address              (* [0x123] *)

type operand = 
  Op_mem_ptr of memory_ptr   |
  Op_reg     of int_register |
  Op_immid   of int          |
  Op_label   of int

type instruction =
  | Mov   of operand * operand
  | Movzx of operand * operand
  | Add   of operand * operand
  | Sub   of operand * operand
  | Imul  of operand * operand
  | Idiv  of operand
  | Test  of operand * operand
  | Cmp   of operand * operand
  | Sete  of operand
  | Setg  of operand
  | Sal   of operand * operand
  | Sar   of operand * operand
  | Or    of operand * operand
  | Xor   of operand * operand
  | Push  of operand
  | Pop   of operand
  | Je    of operand
  | Jmp   of operand
  | Cqo
  | Ret
  | Label of int (* not actually instruction, think about *)

let addr_of_int (v : int) = Addr v

let addr_add_offset (off : int) = function
  | Addr addr -> Addr (addr + off)

let op_is_immid = function
  | Op_immid _ -> true
  | _ -> false

let op_is_reg = function
  | Op_reg _ -> true
  | _ -> false

let op_is_reg8 = function
  | Op_reg AL | Op_reg BL | Op_reg CL | Op_reg DL -> true
  | _ -> false

let op_is_mem_ptr = function
  | Op_mem_ptr _ -> true
  | _ -> false

let op_is_label = function
  | Op_label _ -> true
  | _ -> false