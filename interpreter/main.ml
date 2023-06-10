open Interpreter

let () = 
  if Array.length Sys.argv < 2
    then 
      raise (Failure "Execution mode is not specified")
    else 
      match Sys.argv.(1) with
        | "test" -> print_endline (executor Sys.argv.(2))
        | "repl" -> repl_starter()
        | _      -> failwith "Execution mode is wrong (repl|test)"