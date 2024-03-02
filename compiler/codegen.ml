open Shared.Common
open Shared.Utility

open Common

let fixnum_mask = 0b11
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

let cons_mask = 0b00000111
let cons_tag  = 0b00000001
let car_offset = 8  (* bytes *)
let cdr_offset = 16 (* bytes *)

let gen_nil () =
  [Mov (Op_reg `RAX, Op_immid nil_to_immid)]

let gen_fixnum num =
  let tagged_num = fixnum_to_immid num in
  [Mov (Op_reg `RAX, Op_immid tagged_num)]

let gen_bool (b : bool) =
  [Mov (Op_reg `RAX, Op_immid (bool_to_immid b))]

let gen_cons_val_to_addr = 
  [And (Op_reg `RAX, Op_immid (lnot cons_mask))]

let gen_spilling reg = 
  let _ = Stack.push() in
  [
    Sub (Op_reg `RSP, Op_immid value_size);
    Mov (Op_mem_ptr (FromPlace `RSP), Op_reg reg)
  ]

let gen_stack_remove_top () =
  Stack.pop();
  [Add (Op_reg `RSP, Op_immid value_size)]

let rec gen_func_call fname args =
  let check_args_size args expected = 
    assert (List.length args = expected)
  in

  (* Expects 0 or 1 on RAX *)
  let cmp_res_to_bool = [
    Sal  (Op_reg `RAX, Op_immid bool_shift);
    Or   (Op_reg `RAX, Op_immid bool_tag)
  ] 
  in

  let gen_unar name args = 
    check_args_size args 1;
    let arg_evaluated = gen_expr (List.hd args) in
    List.append 
      arg_evaluated
      (match name with
      | "inc" -> [
        Add (Op_reg `RAX, Op_immid (fixnum_to_immid 1))
      ]
      | "dec" -> [
        Sub (Op_reg `RAX, Op_immid (fixnum_to_immid 1))
      ]
      | "is_zero" -> [
        Test (Op_reg `RAX, Op_reg `RAX);
        Mov  (Op_reg `RAX, Op_immid (fixnum_to_immid 0));
        Sete (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
      ] 
      @ cmp_res_to_bool
      | "is_nil" -> [
        Cmp  (Op_reg `RAX, Op_immid nil_tag);
        Sete (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
      ]
      @ cmp_res_to_bool
      | "is_int" -> [
        And   (Op_reg `RAX, Op_immid fixnum_mask);
        Cmp   (Op_reg `RAX, Op_immid fixnum_tag);
        Sete  (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
      ]
      @ cmp_res_to_bool
      | "is_bool" -> [
        And   (Op_reg `RAX, Op_immid bool_mask);
        Cmp   (Op_reg `RAX, Op_immid bool_tag);
        Sete  (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
      ]
      @ cmp_res_to_bool
      | "not" -> [
        Xor (Op_reg `RAX, Op_immid (1 lsl bool_shift))
        ]
      | "head" -> gen_cons_val_to_addr @ [
        Mov (Op_reg `RAX, Op_mem_ptr (FromPlaceAddOff (`RAX, car_offset)))
      ]
      | "tail" -> gen_cons_val_to_addr @ [
        Mov (Op_reg `RAX, Op_mem_ptr (FromPlaceAddOff (`RAX, cdr_offset)))
      ]
      | _ -> raise (CompilationError "This cannot happen"))
  in

  let gen_bin_op name args =
    check_args_size args 2;
    let arg1_evaluated = gen_expr (List.hd args) in
    let spilling = gen_spilling `RAX in
    let arg2_evaluated = gen_expr (List.nth args 1) in
    let expr_evaluated = (match name with
      | "+" -> [
        Add (Op_reg `RAX, Op_mem_ptr (FromPlace `RSP))
      ]
      | "-" -> [
        Sub (Op_reg `RAX, Op_mem_ptr (FromPlace `RSP))
      ]
      | "*" -> [
        Sar  (Op_mem_ptr (FromPlace `RSP), Op_immid fixnum_shift);
        Imul (Op_reg `RAX, Op_mem_ptr (FromPlace `RSP))
      ]
      | "/" -> [ 
        Cqo;
        Idiv (Op_mem_ptr (FromPlace `RSP));
        Sal (Op_reg `RAX, Op_immid fixnum_shift)
      ]
      | "==" -> [
        Cmp   (Op_reg `RAX, Op_mem_ptr (FromPlace `RSP));
        Sete  (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
      ] 
      @ cmp_res_to_bool
      | ">" -> [
        Cmp   (Op_reg `RAX, Op_mem_ptr (FromPlace `RSP));
        Setg  (Op_reg `AL);
        Movzx (Op_reg `RAX, Op_reg `AL)
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

  (* TODO: now only functions with max 6 int argument are supported *)
  let gen_args_passing (args : c_exp list) : instruction list = 
    assert (List.length args < List.length regs_for_int_arguments);
    
    let (_, res) =
      List.fold_left (fun (i, acc) exp_i -> 
        let reg_for_arg = List.nth regs_for_int_arguments i in
        let instrs =
          gen_expr exp_i @
          [Mov (Op_reg reg_for_arg, Op_reg `RAX)] @
          acc
        in
        (i + 1, instrs)
      ) (0, []) args
    in res
  in

  let gen_save_caller_regs (args_n : int) : instruction list =
    let _ = Stack.pushN args_n in
    let regs = take args_n regs_for_int_arguments in
    List.fold_left (fun acc reg ->
      (Push (Op_reg reg)) :: acc
    ) [] regs
  in

  let gen_restore_caller_regs (args_n : int) : instruction list =
    let _ = Stack.popN args_n in
    let regs = List.rev (take args_n regs_for_int_arguments) in
    List.fold_left (fun acc reg ->
      (Pop (Op_reg reg)) :: acc
    ) [] regs
  in

  let gen_call_site name =
    match Context.get_binding_type name with
      | `GlobalFn _ -> [Call (Op_label name)] (* TODO: only when args num == expected*)
      | `External   -> [Call (Op_label name)] (* RT call *)
      | _ ->
        gen_ident_getter name @
        [Call (Op_reg `RAX)]
  in

  let gen_func_call' (name : name) (callee_args : c_exp list) =
    let caller_args = (Stack.topFrame()).args in
    let caller_args_n = List.length caller_args in

    gen_save_caller_regs caller_args_n    @
    gen_args_passing callee_args          @
    gen_call_site name                    @
    gen_restore_caller_regs caller_args_n
  in

  match fname with
  | Ident(name) -> (match name with
    | "inc" | "dec" | "is_zero" | "not" | "head" | "tail"
    | "is_nil" | "is_int" | "is_bool" -> gen_unar name args
    | "+" | "-" | "*" | "/" | "==" | ">" -> gen_bin_op name args
    | ":" -> gen_func_call' "ULTRA_cons" args
    | name -> gen_func_call' name args)
  | _ -> raise (CompilationError "Not implemented yet")

and gen_let_expr (name : name) (value : c_exp) (body : c_exp) =
  let val_evaluated  = gen_expr value    in
  let _ = Stack.push_local_variable name in
  let _ = Context.register_local    name in 
  let prologue = val_evaluated @ (* Result is located in RAX *)
    [
      Sub (Op_reg `RSP, Op_immid value_size);
      Mov (Op_mem_ptr (FromPlace `RSP), Op_reg `RAX)
    ]
  in 
  let body_evaluated = 
    assert (Option.is_some @@ Stack.get_local_var_offset name);
    gen_expr body
  in
  let _ = Stack.pop() in
  let epilogue = 
    [
      Add (Op_reg `RSP, Op_immid value_size)
    ] 
  in
  prologue @ body_evaluated @ epilogue

and gen_if_expr (cond : c_exp) (exp_true : c_exp) (exp_false : c_exp) =
  let string_of_jmp_label id =
    Printf.sprintf "L%d" id
  in

  let cond_evaluated      = gen_expr cond      in
  let exp_true_evaluated  = gen_expr exp_true  in
  let exp_false_evaluated = gen_expr exp_false in
  let label1 = Stack.alloc_label_id() in
  let label2 = Stack.alloc_label_id() in
  cond_evaluated
  @
  [ Cmp (Op_reg `RAX, Op_immid (bool_to_immid false));
    Je  (Op_label (string_of_jmp_label label1)) ]
  @
  exp_true_evaluated
  @
  [ Jmp (Op_label (string_of_jmp_label label2));
    Label label1 ]
  @
  exp_false_evaluated
  @
  [ Label label2 ]

and gen_ident_getter name =
  let gen_loc_var_getter() =
    let var_place = Stack.get_local_var_place name in
    [
      Mov (Op_reg `RAX, var_place)
    ]
  in
  let gen_get_global_func_address =
    [
      Lea (Op_reg `RAX, Op_mem_ptr (FromLabel name))
    ]
  in
  match Context.get_binding_type name with
  | `GlobalFn _ -> gen_get_global_func_address
  | `Local | `Argument -> gen_loc_var_getter()
  | _ -> raise (CompilationError "Not implemented yet")

and gen_func_prologue =
  [
    Push (Op_reg `RBP);
    Mov (Op_reg `RBP, Op_reg `RSP)
  ]

and gen_func_epilogue =
  [
    Pop (Op_reg `RBP);
    Ret
  ]

and gen_function (name : name) (args : name list) (ast : c_exp) : unit = 
  let _ = Stack.create_new_frame args in
  let _ = Context.register_func name [] in (* Hack for recursion *)
  List.fold_left (fun _ a -> Context.register_arg a; ()) () args;
  let instrs' = gen_expr ast in
  let instrs  = gen_func_prologue @ instrs' @ gen_func_epilogue in
  let _ = Stack.destroy_top_frame () in
  Context.register_func name instrs

and gen_expr = function
  | Ident name -> gen_ident_getter name
  | Literal (Fixnum num) -> gen_fixnum num
  | Literal (Boolean b)  -> gen_bool b
  | Literal (Nil)        -> gen_nil ()
  | If (cond, exp_true, exp_false) -> gen_if_expr cond exp_true exp_false
  | Call (fname, args)   -> gen_func_call fname args
  | Let (name, value, body) -> gen_let_expr name value body
  | ShouldNotReachHere code -> gen_func_call (Ident "ULTRA_runtime_error") [Literal(Fixnum code)]
  | _ -> raise (CompilationError "Not implemented yet")

let gen_def_expr (defexpr : hl_entry) : instruction list = 
  match defexpr with
  | HLExp exp -> gen_expr exp
  | DefFn (name, args, body) -> gen_function name args body; []
  | _ -> raise (CompilationError "Not implemented yet")

let gen_highelevel_exprs (asts : hl_entry list) : unit =
  Stack.create_new_frame [];
  let res = List.fold_left (fun acc ast -> (gen_def_expr ast) @ acc) [] asts in
  let main_body =
    gen_func_prologue @ res @ gen_func_epilogue
  in
  Context.register_func "ultra_entrypoint" main_body