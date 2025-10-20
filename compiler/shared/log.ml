type log_type =
  | TOKENIZER
  | PARSER
  | EXECUTION

let active = [
  TOKENIZER;
  PARSER;
  EXECUTION;
]

let log_type_is_active (tpe: log_type) = List.mem tpe active  

let logMsg (tpe: log_type) (message: string) =
  if log_type_is_active tpe then
    print_endline message