open Common
open Utils

type string_reader =
  {
    mutable sym_idx : int;
    buffer          : string
  };;
  
let constr_string_reader (s : string) : string_reader =
  { sym_idx=0 ; buffer = s ^ "\n" }

let sr_get_symbol (sr : string_reader) : char =
  let sym = String.get sr.buffer sr.sym_idx in
  sr.sym_idx <- sr.sym_idx + 1;
  sym

type input_source = Chan of in_channel | String of string_reader 

type input_stream =
  {
    mutable line_num : int; 
    mutable buffer   : char list; 
    source           : input_source 
  };;

(* read one character from input stream *)
let read_char stream =
  match stream.buffer with
    | [] ->
      let c = begin match stream.source with
        | Chan channel -> input_char channel
        | String sr    -> sr_get_symbol sr
      end
      in
      if c = '\n' 
        then let _ = stream.line_num <- stream.line_num + 1 in c
        else c
    | c::rest -> 
      let _ = stream.buffer <- rest in c

(* place charachter back to stream buffer *)
let unread_char stream c = 
  stream.buffer <- c :: stream.buffer;;

(* remove leading whitespaces in stream *)
let rec eat_whitespaces stream =
  let chr = read_char stream in
  if is_white chr
    then eat_whitespaces stream 
    else unread_char stream chr;
  ();;

(* read fixnum literal from stream *)
let rec read_fixnum stream acc =
  let nc = read_char stream in
  if is_digit nc
    then read_fixnum stream (acc ^ (string_of_char nc))
    else
      let _ = unread_char stream nc in
      Fixnum(int_of_string acc)

(* read boolean literal from stream *)
let read_boolean stream = 
  let nc = read_char stream in
  match nc with
    | 't' -> Boolean true
    | 'f' -> Boolean false
    | inv   -> raise (SyntaxError ("Invalid boolean literal " ^ (string_of_char inv)))

let rec read_symbol stream =
  let nc = read_char stream in
  if is_delim nc
    then let _ = unread_char stream nc in ""
    else string_of_char nc ^ read_symbol stream
  
let rec read_sexpression stream = 
  eat_whitespaces stream;
  let c = read_char stream in

  if c = '('
    then read_list stream
    else

  if is_sym_forwarding_chr c 
    then Symbol(string_of_char c ^ read_symbol stream)
    else 
  
  if is_digit c || (c = '~')
    then read_fixnum stream (string_of_char (if c = '~' then '-' else c))
    else 

  if c = '#'
    then read_boolean stream
    else raise (SyntaxError ("Unexpected char " ^ (string_of_char c)))

and read_list stream =
  eat_whitespaces stream;
  let c = read_char stream in
  if c = ')'
    then Nil
    else 
      let _ = unread_char stream c in
      let car = read_sexpression stream in
      Pair(car, read_list stream);;

(* ================== AST related stuff ================== *)

let rec build_ast (sexpr : value) : exp =
  match sexpr with
  | Primitive _ -> raise (SyntaxError ("Met primitive while AST building"))
  | Fixnum _ | Boolean _ | Nil -> Literal sexpr
  | Symbol s -> Var s
  | Pair _ when is_list sexpr ->
    (match list_of_pairs sexpr with
    | [Symbol "if"; cond; if_true; if_false] ->
      If (build_ast cond, build_ast if_true, build_ast if_false)
    | [Symbol "and"; c1; c2] -> And (build_ast c1, build_ast c2)
    | [Symbol "or"; c1; c2] -> Or (build_ast c1, build_ast c2)
    | [Symbol "val"; Symbol name; e] -> Defexp (Val (name, build_ast e))
    | [Symbol "apply"; fnexp; args] when is_list args ->
      Apply (build_ast fnexp, build_ast args)
    | fnexp::args -> Call (build_ast fnexp, List.map build_ast args)
    | [] -> raise (SyntaxError "Poorly formed expression"))
  | Pair _ -> Literal sexpr