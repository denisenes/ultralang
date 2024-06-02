open Common

let tmp_counter = ref 0

let get_tmp_cnt () =
  let old = !tmp_counter in
  tmp_counter := !tmp_counter + 1;
  Format.sprintf "_tmp%d" old

let rec lower_cond subexps =
  match subexps with
  | [] -> ShouldNotReachHere 0
  | (condition, branch)::tail -> 
    If (lower_ast condition, lower_ast branch, lower_cond tail)

and lower_list_literal exps =
  match exps with
  | [] -> Literal Nil
  | (exp::tail) -> Call (":", [lower_ast exp; lower_list_literal tail])

(* A-normalize function call *)
and normalize_call fname args =
  let temps = List.map (fun _ -> get_tmp_cnt()) args in
  let rec normalize_call' arg_acc = function
    | [] -> 
      let newargs = List.map (fun arg -> Ident arg) temps in
      Call (fname, newargs)
    | (t::acc) -> 
      match arg_acc with
      | [] -> raise (CompilationError "Should not reach here")
      | (ah::at) -> Let (t, lower_ast ah, normalize_call' at acc)
  in
  normalize_call' args temps

(*
 * Call is not normalized when
 * - It's a call to a special binding
 * - All argumets are primitive
 * TODO: make this predicate more precise 
 *)
and should_be_normalized fname args =
  let rec all_args_primitive = function
    | [] -> true
    | (Literal _ :: tl) -> all_args_primitive tl
    | (Ident _ :: tl) -> all_args_primitive tl
    | _ -> false
  in
  not (all_args_primitive args) &&
  match fname with
  | name when List.mem name Common.spec_bindings -> false
  | _ -> true

and lower_ast = function
  (* Lowered nodes *)
  | Call ("apply", args) ->
    Apply (lower_ast @@ List.hd args, List.map lower_ast (List.tl args))
  | Call (funcexp, args) 
    when should_be_normalized funcexp args ->
    normalize_call funcexp args
  | Cond subexps     -> lower_cond subexps
  | ListLiteral exps -> lower_list_literal exps
  (* Trivial cases*)
  | Literal _            as node -> node
  | Ident _              as node -> node
  | ShouldNotReachHere _ as node -> node
  | If (cond, exp1, exp2)  ->
    If (lower_ast cond, lower_ast exp1, lower_ast exp2)
  | Let (name, exp1, exp2) ->
    Let (name, lower_ast exp1, lower_ast exp2)
  | Apply (funcexp, args)  ->
    Apply (lower_ast funcexp, List.map lower_ast args)
  | Lambda (argnames, exp) ->
    Lambda (argnames, lower_ast exp)
  | Call (fname, args) ->
    Call (fname, List.map lower_ast args)

let lower_hl_entry = function
  | DefVal (name, exp) -> DefVal (name, lower_ast exp)
  | DefFn  (name, args, body) -> DefFn (name, args, lower_ast body)
  | HLExp exp -> HLExp (lower_ast exp)