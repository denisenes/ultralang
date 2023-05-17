open Parser
open Printer
open Common
open Lispenv
open Utils

let base_env = 
  let prim_plus = function
    | [Fixnum(a); Fixnum(b)] -> Fixnum(a + b)
    | _ -> raise (EvaluationError("(+ int int)"))
  in
  let prim_pair = function
    | [car; cdr] -> Pair(car, cdr)
    | _ -> raise (EvaluationError("(pair sexpr sexpr)"))
  in
  let new_prim acc (name, func) = 
    bind (name, Primitive(name, func), acc)
  in
  List.fold_left new_prim Nil [
    ("+", prim_plus);
    ("pair", prim_pair)
  ]

let rec eval_sexpression sexpr env =
  match sexpr with
    | Boolean(v) -> (Boolean(v), env)
    | Fixnum(v)  -> (Fixnum(v) , env)
    | Symbol(v)  -> 
      let v_value = lookup (v, env) in
      (v_value, env)
    | Nil        -> (Nil, env)
    (* Special forms evaluation*)
    | Pair(_ , _) when is_list sexpr -> (
      match list_of_pairs sexpr with
      | [Symbol("env")] -> (env, env)
      | [Symbol("if"); cond; iftrue; iffalse] ->
        let (if_res, _) = eval_sexpression 
          (eval_if_expr cond iftrue iffalse env)
          env in
        (if_res, env)
      | [Symbol("val"); Symbol(name); value] ->
        let (exp_res, _) = eval_sexpression value env in
        let env' = bind (name, exp_res, env) in (exp_res, env')
      | Symbol(fn)::args -> 
        (match eval_sexpression (Symbol fn) env with
          | (Primitive(_, f), _) -> (f args, env)
          | _ -> raise (EvaluationError("(apply func args)"))
        )
      | _ -> (raise (EvaluationError("Cannot evaluate list")))
      )
    | _ -> (raise (EvaluationError("Cannot evaluate s-expression")))

and eval_if_expr cond iftrue iffalse env =
  let (condval, _) = eval_sexpression cond env in
  match condval with
    | Boolean(true)  -> iftrue
    | Boolean(false) -> iffalse
    | _ -> raise (EvaluationError("Wrong condition expression"))

let rec loop stream env = 
  print_string "> ";
  flush stdout;
  (* Read *)
  let expr = read_sexpression stream in
  (* AST build*)
  (*let ast = build_ast expr in*)
  (* Eval *)
  let (value, env') = eval_sexpression expr env in
  (* Print *)
  print_value value;
  (* Loop *)
  loop stream env';;

let repl = 
  let stream = { buffer=[]; line_num=0; in_chan=stdin} in
  loop stream base_env