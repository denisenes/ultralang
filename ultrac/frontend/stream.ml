open Shared.Log

let _debug = false


type t = {
  mutable line_num    : int; 
  mutable module_name : string option;
  mutable def_name    : string option;
  mutable buffer      : char Dynarray.t; 
  source              : in_channel; 
}


let create source = {
  line_num = 1;
  module_name = None;
  def_name = None;
  buffer = Dynarray.create ();
  source = source
}


let from_chan stream : char option = 
  In_channel.input_char stream.source


let with_module (s: t) (mname: string option) (action: unit -> 'a) = 
  s.module_name <- mname;
  let res = action() in
  s.module_name <- None;
  res


let with_def (s: t) (dname: string option) (action: unit -> 'a) = 
  s.def_name <- dname;
  let res = action() in
  s.def_name <- None;
  res


let current_pos (s: t): string =
  let mname = Option.value s.module_name ~default:"<Unknown>" in
  let dname = Option.value s.def_name ~default:"<unknown>" in
  Format.sprintf "[%s:%s:%d]" mname dname s.line_num


let read_char stream : char =
  let res = match Dynarray.pop_last_opt stream.buffer with
  | Some c -> c
  | None ->
    begin match from_chan stream with
    | Some c ->
      if c = Token.newline then
        stream.line_num <- stream.line_num + 1;
      c
    | None -> raise End_of_file
    end
  in
  logMsg TOKENIZER ("Char: " ^ String.make 1 res);
  res


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
    match ch with
    | _ when ch |> Token.is_delim -> unread_char stream ch
    | _ when ch |> Token.is_white -> ()
    | _ -> 
      let _ = Buffer.add_char buf ch in
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
    String.fold_right (fun c _ -> unread_char stream c) tok' ()

let peak_token stream =
  let token = read_token stream in
  unread_token stream token;
  token