open Stages
open Shared.Ir
open Shared.Isa
open Shared.Utils
open Shared.Transformer

exception ExecutionException of string

let read_from_file: ir_transformer = function
  | String s -> Channel (open_in s)
  | _ -> invalid_ir_kind "String" "Channel"


let write_image_to_file: ir_transformer = function
  | Image img -> 
    let ch = open_out_bin "out.img" in
    Out_channel.output_bytes ch img;
    Out_channel.close ch;
    Nothing
  | _ -> invalid_ir_kind "Image" "Nothing"


let printer: ir_transformer = fun data ->
  match data with
  | Module m ->
    let ch = Out_channel.open_text (m.name ^ ".ast") in
    let _  = Out_channel.output_string ch @@ show_module_desc m in
    let _  = Out_channel.output_string ch @@ Shared.Type.AdtRegistry.show() in
    let _  = 
      Out_channel.close ch in
      data
  | LLIR llir ->
    let ch = Out_channel.open_text (llir.name ^ ".llir") in
    let _ = Out_channel.output_string ch @@ CUnit.show_unit llir in
    let _ =
      Out_channel.close ch in
      data
  | _ -> raise (InvalidIRKind "Printer input is invalid")


let actions: (stage_data -> stage_data) UString.Map.t = 
  let open UString in 
  Map.empty
  |> Map.add "source/read_stdin"   (fun _ -> Channel (In_channel.stdin))
  |> Map.add "source/read_file"    read_from_file
  |> Map.add "frontend/parser"     Frontend.Parser.parse
  |> Map.add "frontend/parser-asm" Frontend.Parserasm.parse
  |> Map.add "backend/indexer"     Backend.Indexer.index
  |> Map.add "backend/emitter"     Backend.Emitter.emit_image
  |> Map.add "helper/printer"      printer
  |> Map.add "helper/finish"       (fun _ -> Nothing)
  |> Map.add "output/write_image"  write_image_to_file


let stage ?(printable: bool = true) (name: string) : Stage.t =
  let action = UString.Map.find name actions in
  if printable 
    then Stage.constr name action
    else Stage.constr_hidden name action


let driver_compile filename =
  Stages.stages_executor (String filename)
  [|
    stage "source/read_file";
    stage "frontend/parser";
    stage "helper/printer";
    stage "helper/finish";
  |]


let driver_asm filename =
  Stages.stages_executor (String filename)
  [|
    stage "source/read_file";
    stage "frontend/parser-asm";
    stage "backend/indexer";
    stage "helper/printer";
    stage "backend/emitter";
    stage "output/write_image";
  |]


let driver_main (args: string array) =
  let mode = args.(1) in
  let result = match mode with
    | "--compile" | "-c" -> driver_compile args.(2)
    | "--asm" -> driver_asm args.(2)
    | _ -> raise (ExecutionException "Execution mode is not defined")
  in
  match result with
  | Nothing -> print_endline "\nFinished"; ()
  | _ -> raise (ExecutionException "Expected no result on the end of execution")