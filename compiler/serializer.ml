open Shared.Common

open Common

let serialize_reg = function
  | RSP -> "rsp" 
  | RBP -> "rbp"
  | RAX -> "rax" | AL -> "al"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | _ -> raise (CompilationError "Not implemented yet")

let serialize_addr = function
  | Addr a -> (Printf.sprintf "%x" a)

let serialize_mem_ptr = function
  | FromRegSubOff (reg, off) -> "QWORD PTR [" ^ serialize_reg reg ^ "-" ^ string_of_int off ^ "]"
  | FromRegAddOff (reg, off) -> "QWORD PTR [" ^ serialize_reg reg ^ "+" ^ string_of_int off ^ "]"
  | FromReg  reg             -> "QWORD PTR [" ^ serialize_reg reg ^ "]"
  | FromAddr addr            -> "QWORD PTR [" ^ serialize_addr addr ^ "]"

let serialize_operand = function
  | Op_mem_ptr ptr -> serialize_mem_ptr ptr
  | Op_reg r -> serialize_reg r
  | Op_immid v -> string_of_int v

(* TODO operands check *)
let serialize_instr = function
  | Mov (op1, op2)  -> Printf.sprintf 
    "mov %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Add (op1, op2)  -> Printf.sprintf
    "add %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Sub (op1, op2)  -> Printf.sprintf
    "sub %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Imul (op1, op2)  -> Printf.sprintf
    "imul %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Cmp (op1, op2)  -> Printf.sprintf
    "cmp %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Test (op1, op2) -> Printf.sprintf
    "test %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Sete op -> Printf.sprintf
    "sete %s" (serialize_operand op)
  | Sal (op1, op2) -> Printf.sprintf
    "sal %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Sar (op1, op2) -> Printf.sprintf
    "sar %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Or (op1, op2) -> Printf.sprintf
    "or %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Xor (op1, op2) -> Printf.sprintf
    "xor %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Push op -> (match op with
    | Op_reg _ -> Printf.sprintf "push %s" (serialize_operand op)
    | _ -> raise (CompilationError "Wrong push operand"))
  | Pop op -> (match op with
    | Op_reg _ -> Printf.sprintf "pop %s" (serialize_operand op)
    | _ -> raise (CompilationError "Wrong pop operand"))
  | Ret -> "ret"

let serialize_instr_seq (instrs : instruction list) =
  List.fold_left 
    (fun acc instr -> acc ^ "\t" ^ serialize_instr instr ^ "\n") "" instrs