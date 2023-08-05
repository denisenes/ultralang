open Common

let emit_reg = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"

let emit_addr = function
  | Addr a -> (Printf.sprintf "%x" a)

let emit_mem_ptr = function
  | FromReg  reg  -> "[" ^ emit_reg reg   ^ "]"
  | FromAddr addr -> "[" ^ emit_addr addr ^ "]"

let emit_mov_operand = function
  | Op_mem_ptr ptr -> emit_mem_ptr ptr
  | Op_reg r -> emit_reg r
  | Op_immid v -> string_of_int v

let emit_instr = function
  | Mov (op1, op2) -> Printf.sprintf 
    "mov %s, %s" (emit_mov_operand op1) (emit_mov_operand op2)
  | Ret -> "ret"

let emit_instr_seq (instrs : instruction list) =
  List.fold_left 
    (fun acc instr -> acc ^ "\t" ^ emit_instr instr ^ "\n") "" instrs