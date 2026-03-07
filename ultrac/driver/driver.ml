open Stages
open Shared.Ir

exception ExecutionException of string

module StrMap = Map.Make(String)


let read_from_file (str : stage_data) =
  match str with
  | String s -> Channel (open_in s)
  | _ -> raise (End_of_file)


let printer (data: stage_data) =
  match data with
  | Module m ->
    let ch = Out_channel.open_text (m.name ^ ".ast") in
    let _  = Out_channel.output_string ch @@ show_module_desc m in
    let _  = Out_channel.output_string ch @@ Shared.Type.AdtRegistry.show() in
    let _  = 
      Out_channel.close ch in
      Nothing
  | _ -> raise (InvalidIRKind "Printer input is invalid")


let actions: (stage_data -> stage_data) StrMap.t = StrMap.empty
  |> StrMap.add "read_stdin" (fun _ -> Channel (In_channel.stdin))
  |> StrMap.add "read_file"  read_from_file
  |> StrMap.add "parser"     Parser.parse
  |> StrMap.add "printer"    printer
  |> StrMap.add "finish"     (fun _ -> Nothing)


let stage ?(printable: bool = true) (name: string) : Stage.t =
  let action = StrMap.find name actions in
  if printable 
    then Stage.constr name action
    else Stage.constr_hidden name action


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
    | "--compile" | "-c" -> driver_compile args.(2)
    | _ -> raise (ExecutionException "Execution mode is not defined")
  in
  match result with
  | Nothing -> print_endline "\nFinished"; ()
  | _ -> raise (ExecutionException "Expected no result on the end of execution")