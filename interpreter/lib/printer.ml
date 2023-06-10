open Common
open Utils

let rec ast_to_string (node : exp) : string =
  match node with
  | Literal e -> value_to_string e
  | Var name -> name
  | If(cond, l, r) ->
    "(if " ^ ast_to_string cond ^ " " ^ ast_to_string l ^ ast_to_string r ^ ")"
  | And(val1, val2) -> "(and " ^ ast_to_string val1 ^ " " ^ ast_to_string val2 ^ ")"
  | Or(val1, val2) -> "(or " ^ ast_to_string val1 ^ " " ^ ast_to_string val2 ^ ")"
  | Apply(func, e) -> "(apply " ^ ast_to_string func ^ " " ^ ast_to_string e ^ ")"
  | Call(func, args) -> 
    let string_args = (String.concat " " (List.map ast_to_string args)) in
    "(" ^ ast_to_string func ^ " " ^ string_args ^ ")"
  | Defexp(Val(name, e)) -> "(val " ^ name ^ " " ^ ast_to_string e ^ ")"
  | Defexp(Exp exp) -> ast_to_string exp

and value_to_string (sexpr : value) : string =
  match sexpr with
    | Fixnum(value)  -> string_of_int value
    | Boolean(b) -> if b then "#t" else "#f"
    | Symbol(name) -> name
    | Pair(_ , _) -> 
      "(" ^
      (if is_list sexpr then list_to_string sexpr else pair_to_string sexpr)
      ^ ")"

    | Primitive (name,  _) -> "<" ^ name ^ ">"
    | Quote datum -> "\'" ^ value_to_string datum
    | Nil -> "nil"

and list_to_string (list : value) : string = 
  match list with
    | Pair(car, Nil) -> value_to_string car
    | Pair(car, cdr) -> value_to_string car ^ " " ^ list_to_string cdr
    | _ -> raise (PrintError("Something went wrong while printing list"))

and pair_to_string pair = match pair with
  | Pair (car, cdr) -> value_to_string car ^ " . " ^ value_to_string cdr
  | _ -> raise (PrintError("Something went wrong while printing pair"))

let print_value (sexpr : value) : unit =
  print_endline (value_to_string sexpr)