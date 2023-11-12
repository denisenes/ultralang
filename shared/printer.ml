open Common
open Utility

let dbg_mode = false

let rec exp_to_string (node : c_exp) (lvl : int) : string =
  let rec tab_helper l =
    if l == 0 then "" else "\t" ^ tab_helper (l-1)
  in

  let tab l = "\n" ^ tab_helper l in

  match node with
  | Literal e -> 
    (tab lvl) ^ "Lit(" ^ value_to_string e ^ ")"
  | Ident name ->  
    (tab lvl) ^ "Var(" ^ name ^ ")"
  | If(cond, l, r) ->
    (tab lvl) ^ "(Node<If> " ^ exp_to_string cond (lvl+1) ^ exp_to_string l (lvl+1) ^ exp_to_string r (lvl+1) ^ (tab lvl) ^ ")"
  | Apply(func, _) -> 
    (tab lvl) ^ "(Node<Apply> " ^ exp_to_string func (lvl+1) ^ (* ast_to_string e (lvl+1) ^ *) (tab lvl) ^ ")"
  | Call(func, args) -> 
    let string_args = (String.concat " " (List.map (fun x -> exp_to_string x (lvl+1)) args)) in
    (tab lvl) ^ "(Node<Call> " ^ exp_to_string func (lvl+1) ^ string_args ^ (tab lvl) ^ ")"
  | Lambda (args, body) -> 
    (tab lvl) ^ "(Node<Lambda> " ^ args_to_string args ^ exp_to_string body (lvl+1) ^ (tab lvl) ^ ")"
  | Let (var_name, var_val, body) -> 
    (tab lvl) ^ "(Node<Let> " ^ var_name ^ exp_to_string var_val (lvl+1) ^ exp_to_string body (lvl+1) ^ (tab lvl) ^  ")"
  | ShouldNotReachHere code ->
    (tab lvl) ^ "Error(" ^ string_of_int code ^ ")"
  | Cond _ | ListLiteral _ -> raise (EvaluationError "Not implemented yet")

and value_to_string (sexpr : value) : string =
  match sexpr with
    | Fixnum(value)  -> string_of_int value
    | Boolean(b) -> if b then "#t" else "#f"
    | Symbol(name) -> name
    | Pair(_ , _) -> 
      "[" ^
      (if is_list sexpr then list_to_string sexpr else pair_to_string sexpr)
      ^ "]"
    | Closure (args, _, _) -> "<closure>(" ^ args_to_string args ^ ")" 
    | Primitive (name,  _) -> "<" ^ name ^ ">"
    | Quote datum -> "\'" ^ value_to_string datum
    | Nil -> "[]"

and list_to_string (list : value) : string = 
  match list with
    | Pair(car, Nil) -> value_to_string car
    | Pair(car, cdr) -> value_to_string car ^ ", " ^ list_to_string cdr
    | _ -> raise (PrintError("Something went wrong while printing list"))

and args_to_string (args : string list) : string =
  List.fold_left (fun arg acc -> acc ^ " " ^ arg) "" args

and pair_to_string pair = match pair with
  | Pair (car, cdr) -> value_to_string car ^ ", " ^ value_to_string cdr
  | _ -> raise (PrintError("Something went wrong while printing pair"))

let print_value (sexpr : value) : unit =
  print_endline (value_to_string sexpr)

let rec print_env (e : value env) : unit = match e with
  | [] -> ()
  | ((name, value_or_none)::rest) -> 
    (match !value_or_none with
      | Some value -> print_endline (name ^ " " ^ (value_to_string value))
      | None -> print_string "<nothing>")
  ; (print_env rest)

let ast_to_string = function
  | HLExp exp -> "(HLNode<HLExp> " ^ exp_to_string exp 1 ^ ")"
  | DefVal (name, exp) -> "(HLNode<DefVal> " ^ name ^ " " ^ exp_to_string exp 1 ^ ")" 
  | DefFn (name, args, exp) -> "(HLNode<DefFn> " ^ name ^ "[ " ^ args_to_string args ^ "] " ^ exp_to_string exp 1 