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
  Op_immid   of int

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
  | Cqo
  | Ret

let addr_of_int (v : int) = Addr v

let addr_add_offset (off : int) = function
  | Addr addr -> Addr (addr + off)