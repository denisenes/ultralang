open Shared.Common

open Common
open Context

let serialize_reg = function
  | `RSP -> "rsp" 
  | `RBP -> "rbp"
  | `RAX -> "rax" | `EAX -> "eax" | `AL -> "al"
  | `RBX -> "rbx"
  | `RCX -> "rcx"
  | `RDX -> "rdx"
  | `RSI -> "rsi"
  | `RDI -> "rdi"
  | _ -> raise (CompilationError "Not implemented yet")

let serialize_addr = function
  | `Addr a -> (Printf.sprintf "%x" a)

let serialize_mem_ptr = function
  | FromPlaceSubOff (reg, off)       -> "QWORD PTR [" ^ serialize_reg reg ^ "-" ^ string_of_int off ^ "]"
  | FromPlaceAddOff (reg, off)       -> "QWORD PTR [" ^ serialize_reg reg ^ "+" ^ string_of_int off ^ "]"
  | FromPlace (#int_register as reg) -> "QWORD PTR [" ^ serialize_reg reg ^ "]"
  | FromPlace (#address as addr)     -> "QWORD PTR [" ^ serialize_addr addr ^ "]"
  | FromLabel label                  -> "[" ^ label ^ "]"

let serialize_operand = function
  | Op_mem_ptr ptr -> serialize_mem_ptr ptr
  | Op_reg r       -> serialize_reg r
  | Op_immid v     -> string_of_int v
  | Op_label id    -> id

(* TODO operands check according to x64 spec *)
let serialize_instr = function
  | Mov (op1, op2)  -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "mov %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Movzx (op1, op2)  -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "movzx %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Lea (op1, op2) ->
    assert (not @@ op_is_immid op1);
    Printf.sprintf "lea %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Add (op1, op2)  -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "add %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Sub (op1, op2)  -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "sub %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Imul (op1, op2)  -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "imul %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | And (op1, op2) ->
    assert (not @@ op_is_immid op1);
    Printf.sprintf "and %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Idiv op -> 
    Printf.sprintf "idiv %s" (serialize_operand op)
  | Cmp (op1, op2)  -> 
    Printf.sprintf "cmp %s, %s"  (serialize_operand op1) (serialize_operand op2)
  | Test (op1, op2) -> 
    Printf.sprintf "test %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Sete op -> 
    assert (op_is_reg8 op);
    Printf.sprintf "sete %s" (serialize_operand op)
  | Setg op -> 
    assert (op_is_reg8 op);
    Printf.sprintf "setg %s" (serialize_operand op)
  | Sal (op1, op2) -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "sal %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Sar (op1, op2) -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "sar %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Or (op1, op2) -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "or %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Xor (op1, op2) -> 
    assert (not @@ op_is_immid op1);
    Printf.sprintf "xor %s, %s" (serialize_operand op1) (serialize_operand op2)
  | Push op ->
    assert (op_is_reg op);
    Printf.sprintf "push %s" (serialize_operand op)
  | Pop op -> 
    assert (op_is_reg op);
    Printf.sprintf "pop %s" (serialize_operand op)
  | Je op -> 
    assert (op_is_label op);
    Printf.sprintf "je %s" (serialize_operand op)
  | Jmp op -> 
    assert (op_is_label op);
    Printf.sprintf "jmp %s" (serialize_operand op)
  | Call name ->
    Printf.sprintf "call %s" (serialize_operand name)
  | Cqo -> "cqo"
  | Ret -> "ret"
  | Label id -> Printf.sprintf "L%d:" id

let serialize_instr_seq (instrs : instruction list) =
  let ident = function
  | Label _ -> ""
  | _ -> "\t" 
  in
  List.fold_left
    (fun acc instr -> acc ^ (ident instr) ^ serialize_instr instr ^ "\n") "" instrs

let serialize_globl_func name body =
  ".global " ^ name ^ "\n" ^
  ".type " ^ name ^ ", @function\n" ^
  name ^ ":\n" ^ body ^ "\n"

let serialize_private_func name body =
  name ^ ":\n" ^ body ^ "\n"

let serialize_func name body is_global = 
  let body_serialized = serialize_instr_seq body in
  match is_global with
  | true -> serialize_globl_func name body_serialized
  | false -> serialize_private_func name body_serialized

let serialize_all () = 
  Context.fold (fun fname maybe_func acc ->
    match maybe_func with
    | `GlobalFn instrs -> 
      let func_serialized = serialize_func fname instrs true in
      func_serialized ^ acc
    | _ -> acc
  ) !context ""