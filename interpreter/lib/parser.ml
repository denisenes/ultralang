open Common
open Utils

type input_stream =
  { mutable line_num: int; 
    mutable buffer: char list; 
    in_chan: in_channel };;

(* read one character from input stream *)
let read_char stream =
  match stream.buffer with
    | [] ->
      let c = input_char stream.in_chan in
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
      let cdr = read_list stream in
      Pair(car, cdr);;