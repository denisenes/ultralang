open Parser
open Printer
open Shared.Common
open Lispenv
open Utils

let base_env : value env =
  let prim_plus = function
    | [Fixnum(a); Fixnum(b)] -> Fixnum(a + b)
    | _ -> raise (EvaluationError "(+ int int)")
  in
  let prim_minus = function
    | [Fixnum(a); Fixnum(b)] -> Fixnum(a - b)
    | _ -> raise (EvaluationError "(- int int)")
  in
  let prim_mul = function
    | [Fixnum(a); Fixnum(b)] -> Fixnum(a * b)
    | _ -> raise (EvaluationError "(- int int)")
  in
  let prim_div = function
    | [Fixnum(a); Fixnum(b)] -> Fixnum(a / b)
    | _ -> raise (EvaluationError "(- int int)")
  in
  let prim_gt = function
    | [Fixnum(a); Fixnum(b)] -> Boolean(a > b)
    | _ -> raise (EvaluationError "(> int int)")
  in
  let prim_lt = function
    | [Fixnum(a); Fixnum(b)] -> Boolean(a < b)
    | _ -> raise (EvaluationError "(< int int)")
  in
  let prim_eq_val = function
    | [Fixnum(a); Fixnum(b)] -> Boolean(a = b)
    | [Boolean(a); Boolean(b)] -> Boolean(a = b)
    | _ -> raise (EvaluationError "(= (int|boolean) (int|boolean))")
  in
  let prim_pair = function
    | [car; cdr] -> Pair(car, cdr)
    | _ -> raise (EvaluationError("(pair sexpr sexpr)"))
  in
  let prim_car = function
    | [Pair (car, _)] -> car
    | _ -> raise (EvaluationError("(car non-nil-pair)"))
  in
  let prim_cdr = function
    | [Pair (_, cdr)] -> cdr
    | _ -> raise (EvaluationError("(cdr non-nil-pair)"))
  in
  let prim_atom = function
    | [Pair (_, _)] -> Boolean(false)
    | _ -> Boolean(true)
  in
  let prim_eq = function
    | [val1; val2] -> Boolean(val1 = val2)
    | _ -> raise (EvaluationError("(eq val1 val2)"))
  in

  let new_prim acc (name, func) = 
    bind (name, Primitive(name, func), acc)
  in
  let rec prim_list = function
    | [] -> Nil
    | car::cdr -> Pair(car, prim_list cdr)
  in
  List.fold_left new_prim [] [
    ("+", prim_plus);
    ("-", prim_minus);
    ("*", prim_mul);
    ("/", prim_div);
    (">", prim_gt);
    ("<", prim_lt);
    ("=", prim_eq_val);
    ("cons", prim_pair);
    ("list", prim_list);
    ("car", prim_car);
    ("cdr", prim_cdr);
    ("atom?", prim_atom);
    ("eq", prim_eq)
  ]

let rec eval_apply func exprs =
  match func with
  | Primitive (_, func) -> func exprs
  | Closure (args, body, closure_env) -> eval_expr body (bind_bunch args exprs closure_env)
  | _ -> raise (EvaluationError "apply func '(args)) or (func args)")

and eval_expr expr env =
  let rec eval_expr' = function
    | Literal (Quote datum) -> datum
    | Literal literal -> literal
    | Var name -> lookup (name, env)
    | If (cond, if_true,  _) when eval_expr' cond = Boolean true -> 
      eval_expr' if_true
    | If (cond, _, if_false) when eval_expr' cond = Boolean false ->
      eval_expr' if_false
    | If _ -> raise (EvaluationError "(if bool sexpr sexpr)")
    | And (exp1, exp2) -> begin match (eval_expr' exp1, eval_expr' exp2) with
      | (Boolean value1, Boolean value2) -> Boolean (value1 && value2)
      | _ -> raise (EvaluationError "(and bool bool)") end
    | Or (exp1, exp2) -> begin match (eval_expr' exp1, eval_expr' exp2) with
      | (Boolean value1, Boolean value2) -> Boolean (value1 || value2)
      | _ -> raise (EvaluationError "(or bool bool)") end
    | Apply (func, exprs) -> eval_apply (eval_expr' func) (list_of_pairs (eval_expr' exprs))
    | Call (Var "env", []) -> print_env env; Boolean true
    | Call (expr, exprs) -> eval_apply (eval_expr' expr) (List.map eval_expr' exprs)
    | Lambda (args, body) -> Closure (args, body, env)
    | Defexp _ -> raise (EvaluationError "This can't happen")
  in eval_expr' expr 

and eval_defexpr dexpr env  =
  match dexpr with
  | Val (name, expr) -> let value = eval_expr expr env in
    (value, bind (name, value, env))
  | FnDef(name, arg_names, body) -> eval_fndef name arg_names body env
  | Exp expr -> (eval_expr expr env, env)

and eval_fndef name arg_names body env = 
  (* 1) Evaluate lambda to closure *)
  let (formals, body', closure_env) =
    (match eval_expr (Lambda (arg_names, body)) env with
      | Closure (cl_args, cl_body, env) -> (cl_args, cl_body, env)
      | _ -> raise (EvaluationError "Expecting closure")) in
  
  (* 2)  *)
  let loc = mkloc () in
  let clo = Closure (formals, body', bindloc (name, loc, closure_env)) in
  let () = loc := Some clo in
    (clo, bindloc (name, loc, env))
  

and eval_ast ast env = 
  match ast with
  | Defexp dexp -> eval_defexpr dexp env
  | exp -> (eval_expr exp env, env)

let executor (source : string) : unit =
  let sr = constr_string_reader source                     in
  let stream = {buffer=[]; line_num=0; source=(String sr)} in
  let exprs = read_sexprs stream in
  let asts = List.map (fun e -> build_ast e) exprs in

  let last_env : value env ref = ref base_env in (* needed to forward env through exprs during evaluation*)
  let values = List.map 
    (fun ast -> let (v, env) = eval_ast ast !last_env in last_env := env; v) 
    asts in
  let _ = List.map (fun v -> if is_printable_value v then print_value v else ()) values in
  ();;

let rec loop stream env = 
  print_string "> ";
  flush stdout;
  (* Read *)
  let expr = read_sexpression stream in
  (* AST build*)
  let ast = build_ast expr in
  print_endline ("AST: " ^ ast_to_string ast);
  (* Eval *)
  let (value, env') = eval_ast ast env in
  (* Print *)
  print_value value;
  (* Loop *)
  loop stream env'

let repl_starter () = 
  let stream = { buffer=[]; line_num=0; source=(Chan stdin)} in
  loop stream base_env