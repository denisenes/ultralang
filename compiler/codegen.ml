open Shared.Common

open Common

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
  let _ = Stack.push() in
  [
    Sub (Op_reg RSP, Op_immid value_size);
    Mov (Op_mem_ptr (FromReg RSP), Op_reg reg)
  ]

let gen_stack_remove_top () =
  Stack.pop();
  [Add (Op_reg RSP, Op_immid value_size)]

let gen_loc_var_getter name =
  let variable_location = Stack.get_local_var_offset name in
  match variable_location with
  | Some offset ->
    [
      Mov (Op_reg RAX, Op_mem_ptr (FromRegSubOff (RBP, offset)))
    ]
  | None -> raise (CompilationError ("Undefined symbol " ^ name))

let rec gen_func_call fname args =
  let check_args_size args expected = 
    assert (List.length args = expected)
  in

  (* Expects 0 or 1 on RAX *)
  let cmp_res_to_bool = [
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
      | "zero?" -> [
        Test (Op_reg RAX, Op_reg RAX);
        Mov  (Op_reg RAX, Op_immid (fixnum_to_immid 0));
        Sete (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ] 
      @ cmp_res_to_bool
      | "null?" -> [
        Cmp  (Op_reg RAX, Op_immid nil_tag);
        Sete (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ]
      @ cmp_res_to_bool
      | "int?" -> [
        Cmp  (Op_reg RAX, Op_immid fixnum_tag);
        Sete (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ]
      @ cmp_res_to_bool
      | "bool?" -> [
        Cmp  (Op_reg RAX, Op_immid bool_tag);
        Sete (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ]
      @ cmp_res_to_bool
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
      | "/" -> [ 
        Cqo;
        Idiv (Op_mem_ptr (FromReg RSP));
        Sal (Op_reg RAX, Op_immid fixnum_shift)
      ]
      | "=" -> [
        Cmp   (Op_reg RAX, Op_mem_ptr (FromReg RSP));
        Sete  (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ] 
      @ cmp_res_to_bool
      | ">" -> [
        Cmp   (Op_reg RAX, Op_mem_ptr (FromReg RSP));
        Setg (Op_reg AL);
        Movzx (Op_reg RAX, Op_reg AL)
      ] 
      @ cmp_res_to_bool
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
    | "+" | "-" | "*" | "/" | "=" | ">" -> gen_bin_op name args
    | _ -> raise (CompilationError "Not implemented yet"))
  | _ -> raise (CompilationError "Not implemented yet")

and gen_let_expr (name : name) (value : exp) (body : exp) =
  let val_evaluated  = gen_expr value    in
  let _ = Stack.push_local_variable name in
  let prologue = val_evaluated @ (* Result is located in RAX *)
    [
      Sub (Op_reg RSP, Op_immid value_size);
      Mov (Op_mem_ptr (FromReg RSP), Op_reg RAX)
    ]
  in 
  let body_evaluated = 
    assert (Option.is_some @@ Stack.get_local_var_offset name);
    gen_expr body
  in
  let _ = Stack.pop() in
  let epilogue = 
    [
      Add (Op_reg RSP, Op_immid value_size)
    ] 
  in
  prologue @ body_evaluated @ epilogue

and gen_if_expr (cond : exp) (exp_true : exp) (exp_false : exp) =
  let cond_evaluated      = gen_expr cond      in
  let exp_true_evaluated  = gen_expr exp_true  in
  let exp_false_evaluated = gen_expr exp_false in
  let label1 = Stack.alloc_label_id() in
  let label2 = Stack.alloc_label_id() in
  cond_evaluated
  @
  [ Cmp (Op_reg RAX, Op_immid (bool_to_immid false));
    Je  (Op_label label1) ]
  @
  exp_true_evaluated
  @
  [ Jmp (Op_label label2);
    Label label1 ]
  @
  exp_false_evaluated
  @
  [ Label label2 ]

and gen_expr = function
  | Var name -> gen_loc_var_getter name
  | Literal (Fixnum num) -> gen_fixnum num
  | Literal (Boolean b)  -> gen_bool b
  | Literal (Nil)        -> gen_nil ()
  | If (cond, exp_true, exp_false) -> gen_if_expr cond exp_true exp_false
  | Call (fname, args)   -> gen_func_call fname args
  | Let (name, value, body) -> gen_let_expr name value body
  | _ -> raise (CompilationError "Not implemented yet")

let gen_func_prologue =
  [
    Push (Op_reg RBP);
    Mov (Op_reg RBP, Op_reg RSP)
  ]

let gen_func_epilogue =
  [
    Pop (Op_reg RBP);
    Ret
  ]

let gen_exprs (asts : exp list) =
  let res = List.map (fun ast -> gen_expr ast) asts in
  [gen_func_prologue] @ res @ [gen_func_epilogue]