open Common
open Utils

let rec print_sexpression sexpr =
  match sexpr with
    | Fixnum(value)  -> print_int value
    | Boolean(b) -> print_string (if b then "#t" else "#f")
    | Symbol(name) -> print_string name
    | Pair(_ , _) -> 
      print_string "(";
      if is_list sexpr then print_list sexpr else print_pair sexpr;
      print_string ")"
    | Nil -> print_string "nil"

and print_list list = 
  match list with
    | Pair(car, Nil) -> print_sexpression car
    | Pair(car, cdr) -> 
      print_sexpression car;
      print_string " ";
      print_list cdr;
    | _ -> raise (PrintError("Something went wrong while printing list"))

and print_pair pair = match pair with
  | Pair (car, cdr) -> 
    print_sexpression car;
    print_string ".";
    print_sexpression cdr;
  | _ -> raise (PrintError("Something went wrong while printing pair"))