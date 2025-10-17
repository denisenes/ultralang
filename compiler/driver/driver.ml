open Stages
open Shared.Ir

exception ExecutionException of string

module StrMap = Map.Make(String)


let read_from_file (str : stage_data) =
  match str with
  | String s -> Channel (open_in s)
  | _ -> raise (End_of_file)


let actions: (stage_data -> stage_data) StrMap.t = StrMap.empty
  |> StrMap.add "repl_draw"  (fun _ -> print_endline ">> "; Nothing)
  |> StrMap.add "repeat"     (fun _ -> Nothing) (* Control stage *)
  |> StrMap.add "read_stdin" (fun _ -> Channel (In_channel.stdin))
  |> StrMap.add "read_file"  read_from_file
  |> StrMap.add "parser"     Parser.parse
  |> StrMap.add "printer"    (fun x -> x) (* TODO implement *)
  |> StrMap.add "finish"     (fun _ -> Nothing)


let stage ?(printable: bool = false) (name: string) : Stage.t =
  let action = StrMap.find name actions in
  if printable 
    then Stage.constr name action
    else Stage.constr_hidden name action


let driver_repl () = 
  Stages.stages_executor_no_input
  [|
    stage "repl_draw";
    stage "read_stdin";
    stage "parser";
    stage "printer";
    stage "repeat";
  |]


let driver_compile filename =
  Stages.stages_executor (String filename)
  [|
    stage "read_file";
    stage "parser";
    stage "printer";
  |]


let driver_main (args: string array) =
  let mode = args.(1) in
  let result = match mode with
    | "repl" -> driver_repl ()
    | "compile" -> driver_compile args.(2)
    | _ -> raise (ExecutionException "Execution mode is not defined")
  in
  match result with
  | Nothing -> print_endline "\nFinished"; ()
  | _ -> raise (ExecutionException "Expected no result on the end of execution")