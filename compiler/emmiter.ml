open Shared.Common

open Common

let emit_reg = function
  | RAX -> "rax" | AL -> "al"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | _ -> raise (CompilationError "Not implemented yet")

let emit_addr = function
  | Addr a -> (Printf.sprintf "%x" a)

let emit_mem_ptr = function
  | FromReg  reg  -> "[" ^ emit_reg reg   ^ "]"
  | FromAddr addr -> "[" ^ emit_addr addr ^ "]"

let emit_operand = function
  | Op_mem_ptr ptr -> emit_mem_ptr ptr
  | Op_reg r -> emit_reg r
  | Op_immid v -> string_of_int v

(* TODO operands check *)
let emit_instr = function
  | Mov (op1, op2)  -> Printf.sprintf 
    "mov %s, %s"  (emit_operand op1) (emit_operand op2)
  | Add (op1, op2)  -> Printf.sprintf
    "add %s, %s"  (emit_operand op1) (emit_operand op2)
  | Sub (op1, op2)  -> Printf.sprintf
    "sub %s, %s"  (emit_operand op1) (emit_operand op2)
  | Cmp (op1, op2)  -> Printf.sprintf
    "cmp %s, %s"  (emit_operand op1) (emit_operand op2)
  | Test (op1, op2) -> Printf.sprintf
    "test %s, %s" (emit_operand op1) (emit_operand op2)
  | Sete op -> Printf.sprintf
    "sete %s" (emit_operand op)
  | Sal (op1, op2) -> Printf.sprintf
    "sal %s, %s" (emit_operand op1) (emit_operand op2)
  | Or (op1, op2) -> Printf.sprintf
    "or %s, %s" (emit_operand op1) (emit_operand op2)
  | Xor (op1, op2) -> Printf.sprintf
    "xor %s, %s" (emit_operand op1) (emit_operand op2)
  | Ret -> "ret"

let emit_instr_seq (instrs : instruction list) =
  List.fold_left 
    (fun acc instr -> acc ^ "\t" ^ emit_instr instr ^ "\n") "" instrs