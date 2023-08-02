open Common

let gen_reg = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"

let gen_addr = function
  | Addr a -> (Printf.sprintf "%x" a)

let gen_mem_ptr = function
  | FromReg  reg  -> "[" ^ gen_reg reg   ^ "]"
  | FromAddr addr -> "[" ^ gen_addr addr ^ "]"

let gen_mov_operand = function
  | Op_mem_ptr ptr -> gen_mem_ptr ptr
  | Op_reg r -> gen_reg r
  | Op_immid v -> string_of_int v

let gen_instr = function
  | Mov (op1, op2) -> Printf.sprintf 
    "mov %s, %s" (gen_mov_operand op1) (gen_mov_operand op2)
  | Ret -> "ret"

let gen_instr_seq (instrs : instruction list) =
  List.fold_left 
    (fun acc instr -> acc ^ gen_instr instr ^ "\n") "" instrs