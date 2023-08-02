type int_register = RAX | RBX | RCX | RDX | RSI | RDI

type address = Addr of int

type memory_ptr = 
  FromReg  of int_register |
  FromAddr of address

type mov_operand = 
  Op_mem_ptr of memory_ptr   | 
  Op_reg     of int_register |
  Op_immid   of int

type instruction =
  | Mov of mov_operand * mov_operand
  | Ret

let addr_of_int (v : int) = Addr v

let addr_add_offset (off : int) = function
  | Addr addr -> Addr (addr + off)