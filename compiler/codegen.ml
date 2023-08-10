open Shared.Common

open Common
open Stack

let fixnum_tag = 0b00
let fixnum_shift = 2
let fixnum_to_immid num = (num lsl fixnum_shift) lor fixnum_tag

let bool_mask  = 0b1111111
let bool_tag   = 0b0011111
let bool_shift = 7
let bool_to_immid = function
  | true  -> (1 lsl bool_shift) lor bool_tag
  | false -> (0 lsl bool_shift) lor bool_tag

let nil_mask = 0b11111111
let nil_tag  = 0b00101111
let nil_to_immid = nil_tag

let gen_nil () =
  [Mov (Op_reg RAX, Op_immid nil_to_immid)]

let gen_fixnum num =
  let tagged_num = fixnum_to_immid num in
  [Mov (Op_reg RAX, Op_immid tagged_num)]

let gen_bool (b : bool) =
  [Mov (Op_reg RAX, Op_immid (bool_to_immid b))]

let gen_spilling reg = 
  let _ = push_spilled_val() in
  [
    Sub (Op_reg RSP, Op_immid value_size);
    Mov (Op_mem_ptr (FromReg RSP), Op_reg reg)
  ]

let gen_stack_remove_top () =
  pop_spilled_val ();
  [Add (Op_reg RSP, Op_immid value_size)]

let rec gen_func_call fname args =
  let check_args_size args expected = 
    if List.length args = expected
      then ()
      else raise (CompilationError "Wrong args num in application")
  in

  let cmp_res_to_bool = [
    Sete (Op_reg AL);
    Sal  (Op_reg RAX, Op_immid bool_shift);
    Or   (Op_reg RAX, Op_immid bool_tag)
  ] 
  in

  let gen_unar name args = 
    check_args_size args 1;
    let arg_evaluated = gen_expr (List.hd args) in
    List.append 
      arg_evaluated
      (match name with
      | "inc" -> [
        Add (Op_reg RAX, Op_immid (fixnum_to_immid 1))
      ]
      | "dec" -> [
        Sub (Op_reg RAX, Op_immid (fixnum_to_immid 1))
      ]
      | "zero?" -> List.append [
        Test (Op_reg RAX, Op_reg RAX);
        Mov  (Op_reg RAX, Op_immid (fixnum_to_immid 0));
      ] 
      cmp_res_to_bool
      | "null?" -> List.append [
        Cmp  (Op_reg RAX, Op_immid nil_tag);
      ]
      cmp_res_to_bool
      | "int?" -> List.append [
        Cmp  (Op_reg RAX, Op_immid fixnum_tag);
      ]
      cmp_res_to_bool
      | "bool?" -> List.append [
        Cmp  (Op_reg RAX, Op_immid bool_tag);
      ]
      cmp_res_to_bool
      | "not" -> [
        Xor (Op_reg RAX, Op_immid (1 lsl bool_shift))
        ]
      | _ -> raise (CompilationError "This cannot happen"))
  in

  let gen_bin_op name args =
    check_args_size args 2;
    let arg1_evaluated = gen_expr (List.hd args) in
    let spilling = gen_spilling RAX in
    let arg2_evaluated = gen_expr (List.nth args 1) in
    let expr_evaluated = (match name with
      | "+" -> [
        Add (Op_reg RAX, Op_mem_ptr (FromReg RSP))
      ]
      | "-" -> [
        Sub (Op_reg RAX, Op_mem_ptr (FromReg RSP))
      ]
      | "*" -> [
        Sar  (Op_mem_ptr (FromReg RSP), Op_immid fixnum_shift);
        Imul (Op_reg RAX, Op_mem_ptr (FromReg RSP))
      ]
      | _ -> raise (CompilationError "Not implemented yet")) in
    List.concat [
      arg2_evaluated;
      spilling;
      arg1_evaluated;
      expr_evaluated;
      gen_stack_remove_top()
    ]
  in

  match fname with
  | Var(name) -> (match name with
    | "inc" | "dec" | "zero?" | "not" 
    | "null?" | "int?" | "bool?" -> gen_unar name args
    | "+" | "-" | "*" -> gen_bin_op name args
    | _ -> raise (CompilationError "Not implemented yet"))
  | _ -> raise (CompilationError "Not implemented yet")

and gen_expr = function
  | Literal (Fixnum num) -> gen_fixnum num
  | Literal (Boolean b)  -> gen_bool b
  | Literal (Nil)        -> gen_nil ()
  | Call (fname, args)   -> gen_func_call fname args
  | _ -> raise (CompilationError "Not implemented yet")

let gen_exprs (asts : exp list) =
  let res = List.map (fun ast -> gen_expr ast) asts in
  res @ [[Ret]]