open Parser
open Printer
open Common
open Lispenv
open Utils

let base_env = 
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
  let prim_pair = function
    | [car; cdr] -> Pair(car, cdr)
    | _ -> raise (EvaluationError("(pair sexpr sexpr)"))
  in
  let new_prim acc (name, func) = 
    bind (name, Primitive(name, func), acc)
  in
  let rec prim_list = function
    | [] -> Nil
    | car::cdr -> Pair(car, prim_list cdr)
  in
  List.fold_left new_prim Nil [
    ("+", prim_plus);
    ("-", prim_minus);
    ("*", prim_mul);
    ("/", prim_div);
    ("pair", prim_pair);
    ("list", prim_list)
  ]

let eval_apply func exprs =
  match func with
  | Primitive (_, func) -> func exprs
  | _ -> raise (EvaluationError "apply prim '(args)) or (prim args)")

let rec eval_expr expr env =
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
    | Call (Var "env", []) -> env
    | Call (expr, exprs) -> eval_apply (eval_expr' expr) (List.map eval_expr' exprs)
    | Defexp _ -> raise (EvaluationError "This can't happen")
  in eval_expr' expr 

and eval_defexpr dexpr env  =
  match dexpr with
  | Val (name, expr) -> let value = eval_expr expr env in
    (value, bind (name, value, env))
  | Exp expr -> (eval_expr expr env, env)

and eval_ast ast env = 
  match ast with
  | Defexp dexp -> eval_defexpr dexp env
  | exp -> (eval_expr exp env, env)

let executor (source : string) : string =
  let sr = constr_string_reader source                     in
  let stream = {buffer=[]; line_num=0; source=(String sr)} in
  let expr = read_sexpression stream                       in
  let ast = build_ast expr                                 in
  let (value, _) = eval_ast ast base_env                   in
  value_to_string value

let rec loop stream env = 
  print_string "> ";
  flush stdout;
  (* Read *)
  let expr = read_sexpression stream in
  (* AST build*)
  let ast = build_ast expr in
  (* Eval *)
  let (value, env') = eval_ast ast env in
  (* Print *)
  print_value value;
  (* Loop *)
  loop stream env'

let repl_starter () = 
  let stream = { buffer=[]; line_num=0; source=(Chan stdin)} in
  loop stream base_env