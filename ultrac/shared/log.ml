type log_type =
  | TOKENIZER
  | PARSER
  | EXECUTION

let active = [
  EXECUTION;
]

let log_type_is_active (tpe: log_type) = List.mem tpe active  

let logMsg (tpe: log_type) (message: string) =
  if log_type_is_active tpe then
    print_endline message;
    Out_channel.flush_all()

let logScoped (tpe: log_type) (what: string) (action: unit -> 'a) =
  let _ = logMsg tpe @@ Format.sprintf "Starting %s" what in
  let res = action () in
  let _ = logMsg tpe @@ Format.sprintf "Ended %s" what in
  res
