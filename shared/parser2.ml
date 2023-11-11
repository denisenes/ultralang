open Common
open Utility

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

let sr_peek (sr : string_reader) : char =
  if sr.sym_idx >= String.length sr.buffer
    then raise End_of_file
    else String.get sr.buffer sr.sym_idx

type input_source = Chan of in_channel | String of string_reader 

type input_stream =
  {
    mutable line_num : int; 
    mutable buffer   : char list; 
    source           : input_source 
  };;

let new_input_stream (sr : string_reader) =
  {buffer=[]; line_num=0; source=(String sr)}

(* peek next character without stream modification *)
let peek_char stream =
  match stream.buffer with
    | [] ->
      let c = begin match stream.source with
        | Chan _ -> ' ' (* TODO: peek from channel *)
        | String sr -> sr_peek sr 
      end in c
    | c::_ -> c

let eof_in_stream stream =
  try
    let _ = peek_char stream in false
  with
    End_of_file -> true

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

let rec read_token stream read_delim : string =
  let rec read_token' (tok : string) =
    let ch = read_char stream in
    if is_white ch || (is_delim ch && not read_delim) then begin 
      unread_char stream ch;
      tok
    end else
      read_token' (tok ^ string_of_char ch)
  in
  eat_whitespaces stream;
  let s = (read_token' "") in
  Printf.printf "(Token read: %s)\n" s;
  s

and unread_token stream (tok : string) : unit =
  Printf.printf "(Token unread: %s)\n" tok;
  if tok = "" then () else
  let tok' = tok ^ " " in (* because we don't want to join this token with the next *)
  let chs = List.init (String.length tok') (String.get tok') in
  List.fold_right (fun c _ -> unread_char stream c; ()) chs ()

(* remove leading whitespaces in stream *)
and eat_whitespaces stream =
  if eof_in_stream stream then () else
    
  let chr = read_char stream in
  if is_white chr
    then eat_whitespaces stream 
    else unread_char stream chr;
  ();;

(* === Parser === *)

let check_opening_delim stream del : bool =
  let opening = read_char stream in
  match opening with
  | '(' | '[' when opening = del -> true
  | _ -> begin
    unread_char stream opening; 
    false
  end 

let check_closing_delim stream del =
  let closing = read_char stream in
  match closing with
  | ')' | ']' when closing = del -> ()
  | _ -> raise (SyntaxError ("Unexpected ending of an expression: " ^ string_of_char closing))


let rec parse_exp stream =
  eat_whitespaces stream;
  let ch = read_char stream in

  if ch = '(' then
    let iexp = parse_infix_exp stream in
    check_closing_delim stream ')';
    iexp
  else

  if ch = '#' then
    let tok = read_token stream false in
    match tok with
    | "True"  -> Literal (Boolean true)
    | "False" -> Literal (Boolean false)
    | _ -> raise (SyntaxError "Error while parsing boolean literal")
  else 

  if is_digit ch then begin
    unread_char stream ch;
    let tok = read_token stream false in
    Literal (Fixnum (int_of_string tok))
  end else

  if ch = '[' then
    let ch' = read_char stream in
    match ch' with
    | ']' -> Literal Nil
    | _ -> raise (SyntaxError "Not implemented yet")
  else 

  raise (SyntaxError ("Expression is not supported, ch = " ^ string_of_char ch))

and parse_infix_exp stream =
  eat_whitespaces stream;
  let infix_op = ["*"; "+"; "-"; "/"; ">"; "<"; "="; ":"] in
  let is_infix_op c = List.mem c infix_op in

  let exp1 = parse_exp stream in
  
  let tok = read_token stream false in
  if is_infix_op tok then
    let exp2 = parse_exp stream in
    Call (Ident tok, [exp1; exp2])
  else begin
    unread_token stream tok; 
    exp1
  end

let parse_hl_exp stream =
  let parse_val_def () =
    HLExp (Literal Nil)
  in
  let parse_func_def () =
    HLExp (Literal Nil)
  in

  let tok = read_token stream false in
  let hl_exp = match tok with
  | "val" -> parse_val_def ()
  | "fn"  -> parse_func_def ()
  | _ -> unread_token stream tok; 
         HLExp (parse_infix_exp stream)
  in
  
  let maybe_hl_delim = read_token stream true in
  match maybe_hl_delim with
  | ";;" -> hl_exp
  | _ -> raise (SyntaxError ("Error at the end of highlevel expression, tok = " ^ maybe_hl_delim))

let rec parse_program stream : program =
  eat_whitespaces stream;
  if eof_in_stream stream 
    then []
    else let hl_exp = parse_hl_exp stream in
      hl_exp::(parse_program stream) 

(* === High level === *)

let read_src_from_file filename =
  Core.In_channel.read_all filename

let parse_from_string src : program =
  let sr = constr_string_reader src in
  let stream = new_input_stream sr in
  parse_program stream

let parse_from_file filename : program =
  let src = read_src_from_file filename in
  parse_from_string src