open Common
open Utils

let rec ast_to_string (node : exp) : string =
  match node with
  | Literal e -> "Lit(" ^ value_to_string e ^ ")"
  | Var name -> "Var(" ^ name ^ ")"
  | If(cond, l, r) ->
    "(Node<If> " ^ ast_to_string cond ^ " " ^ ast_to_string l ^ ast_to_string r ^ ")"
  | And(val1, val2) -> "(Node<And> " ^ ast_to_string val1 ^ " " ^ ast_to_string val2 ^ ")"
  | Or(val1, val2) -> "(Node<Or> " ^ ast_to_string val1 ^ " " ^ ast_to_string val2 ^ ")"
  | Apply(func, e) -> "(Node<Apply> " ^ ast_to_string func ^ " " ^ ast_to_string e ^ ")"
  | Call(func, args) -> 
    let string_args = (String.concat " " (List.map ast_to_string args)) in
    "(" ^ ast_to_string func ^ " " ^ string_args ^ ")"
  | Lambda (args, body) -> "(Node<Lambda> " ^ args_to_string args ^ ast_to_string body ^ ")"
  | Defexp(Val(name, e)) -> "(Node<Val> " ^ name ^ " " ^ ast_to_string e ^ ")"
  | Defexp(Exp exp) -> ast_to_string exp
  | Defexp(FnDef(name, arg_names, body)) ->"(Node<FnDef> " ^ name ^ " " ^ args_to_string arg_names ^ ast_to_string body ^ ")"

and value_to_string (sexpr : value) : string =
  match sexpr with
    | Fixnum(value)  -> string_of_int value
    | Boolean(b) -> if b then "#t" else "#f"
    | Symbol(name) -> name
    | Pair(_ , _) -> 
      "(" ^
      (if is_list sexpr then list_to_string sexpr else pair_to_string sexpr)
      ^ ")"
    | Closure (args, _, _) -> "<closure>(" ^ args_to_string args ^ ")" 
    | Primitive (name,  _) -> "<" ^ name ^ ">"
    | Quote datum -> "\'" ^ value_to_string datum
    | Nil -> "nil"

and list_to_string (list : value) : string = 
  match list with
    | Pair(car, Nil) -> value_to_string car
    | Pair(car, cdr) -> value_to_string car ^ " " ^ list_to_string cdr
    | _ -> raise (PrintError("Something went wrong while printing list"))

and args_to_string (args : string list) : string =
  List.fold_left (fun arg acc -> acc ^ " " ^ arg) "" args

and pair_to_string pair = match pair with
  | Pair (car, cdr) -> value_to_string car ^ " . " ^ value_to_string cdr
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