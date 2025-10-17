open Shared.Log

let _debug = false

type t = {
  mutable line_num : int; 
  mutable buffer   : char Dynarray.t; 
  source           : in_channel; 
}


let create source = {
  line_num = 1;
  buffer = Dynarray.create ();
  source = source
}


let from_chan stream : char option = 
  In_channel.input_char stream.source


let read_char stream : char =
  match Dynarray.pop_last_opt stream.buffer with
  | Some c -> c
  | None ->
    begin match from_chan stream with
    | Some c ->
      if c = Token.newline then
        stream.line_num <- stream.line_num + 1;
      c
    | None -> raise End_of_file
    end


let unread_char stream c : unit = 
  Dynarray.add_last stream.buffer c


let peek_char stream : char =
  match Dynarray.pop_last_opt stream.buffer with
  | Some c -> Dynarray.add_last stream.buffer c; c
  | None ->
    begin match from_chan stream with
    | Some c -> 
      Dynarray.add_last stream.buffer c;
      c
    | None -> raise End_of_file
    end


let rec skip_whitespaces stream =
  let c = read_char stream in
  if Token.is_white c
    then skip_whitespaces stream 
    else unread_char stream c


let read_token stream: string =
  let buf = Buffer.create 8 in
  let rec read_token' (): unit =
    let ch = read_char stream in
    if Token.is_white ch || Token.is_delim ch then
      unread_char stream ch
    else
      Buffer.add_char buf ch;
      read_token' ()
  in
  skip_whitespaces stream;
  read_token' ();
  let token = Buffer.contents buf in
  logMsg TOKENIZER ("[Stream] read token: " ^ token); 
  token


let unread_token stream (tok : string) : unit =
  if tok = "" then 
    () 
  else
    logMsg TOKENIZER ("[Stream] token unread" ^ tok);
    let tok' = tok ^ " " in (* because we don't want to join this token with the next *)
    String.iter (fun c -> unread_char stream c) tok'


let peak_token stream =
  let token = read_token stream in
  unread_token stream token;
  token