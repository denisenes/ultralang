open Common

open Shared.Common

type frame_descriptor = {
  mutable rsp_offset    : int; (* rsp = rbp + offset *)
  mutable values        : (string * int) list;
  mutable label_counter : int;
  args                  : name list
}

type abstract_stack = frame_descriptor list

let global_stack : abstract_stack ref = ref []

let create_new_frame args : unit =
  let new_frame = { rsp_offset=0; values=[]; label_counter=0; args } in
  global_stack := new_frame::!global_stack

let push () = 
  match !global_stack with
  | [] -> raise (CompilationError "No frame descriptor")
  | (frame::_) ->
    frame.rsp_offset <- frame.rsp_offset + value_size;
    frame.rsp_offset;;

let pop () =
  match !global_stack with
  | [] -> raise (CompilationError "No frame descriptor")
  | (frame::_) ->
    frame.rsp_offset <- frame.rsp_offset - value_size;
    ();;

let push_local_variable name =
  let index = push() in
  let frame = List.hd (!global_stack) in
  frame.values <- (name, index)::frame.values

let get_local_var_offset name : int option =
  let frame = List.hd (!global_stack) in
  let maybe_offset = List.find_opt (fun (name', _) -> name = name') frame.values in
  match maybe_offset with
  | None -> None
  | Some (_, off) -> Some off

let alloc_label_id () =
  let frame = List.hd !global_stack in
  let old = frame.label_counter in
  frame.label_counter <- frame.label_counter + 1;
  old