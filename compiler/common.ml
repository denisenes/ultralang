type int_register = 
  RAX | EAX | AX | AL |
  RBX | EBX | BX | BL |
  RCX | ECX | CX | CL |
  RDX | EDX | DX | DL |
  RSI | ESI |
  RDI | EDI

type address = Addr of int

type memory_ptr = 
  FromReg  of int_register |
  FromAddr of address

type operand = 
  Op_mem_ptr of memory_ptr   | 
  Op_reg     of int_register |
  Op_immid   of int

type instruction =
  | Mov  of operand * operand
  | Add  of operand * operand
  | Sub  of operand * operand
  | Test of operand * operand
  | Cmp  of operand * operand
  | Sete of operand
  | Sal  of operand * operand
  | Or   of operand * operand
  | Xor  of operand * operand
  | Ret

let addr_of_int (v : int) = Addr v

let addr_add_offset (off : int) = function
  | Addr addr -> Addr (addr + off)