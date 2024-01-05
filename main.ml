open Interpreter

let () = 
  if Array.length Sys.argv < 2
    then 
      raise (Failure "Execution mode is not specified")
    else 
      match Sys.argv.(1) with
        | "test" -> executor Sys.argv.(2)
        | "repl" -> repl_starter()
        (* | "compile" -> compiler Sys.argv.(2) *)
        | _      -> failwith "Execution mode is wrong (repl|compile|test)"