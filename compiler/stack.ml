open Common

open Shared.Common

let debug = false

(* This is supposed to save abstract function frame context when
   compiler moves to codegen of nested function definition *)
type frame_descriptor = {
  mutable rsp_offset    : int; (* rsp = rbp + rsp_offset *)
  mutable stack_values  : (string * int) list; (* TODO: Map *)
  args                  : name list
}

let label_counter : int ref = ref 0

type abstract_stack = frame_descriptor list

let global_stack : abstract_stack ref = ref []

let create_new_frame args : unit =
  assert (List.length args <= List.length regs_for_int_arguments);

  let new_frame = { rsp_offset=0; stack_values=[]; args } in
  global_stack := new_frame::!global_stack
;;

let destroy_top_frame () : unit =
  assert (List.length !global_stack > 0);
  global_stack := List.tl !global_stack

let topFrame () : frame_descriptor =
  assert (List.length !global_stack > 0);
  List.hd !global_stack

(* 
  Moves offset of abstract rsp inside frame_descriptor
  but actual rsp advance should be made inside codegen func (!!!)
  It can be done by 2 ways:
    * Substruction from RSP
    * Using push instructions
*)
let pushN num msg =
  match !global_stack with
  | [] -> raise (CompilationError "No frame descriptor")
  | (frame::_) ->
    frame.rsp_offset <- frame.rsp_offset + (value_size * num);
    if debug then
      Printf.printf "Push (_, %d) %s\n" frame.rsp_offset msg;
    frame.rsp_offset
;;

let push msg = 
  pushN 1 msg
;;

(* 
  Moves offset of abstract rsp inside frame_descriptor
  but actual rsp retreat should be made inside codegen func (!!!)
  It can be done by 2 ways:
    * Addition to RSP
    * Using pop instructions
*)
let popN num msg =
  match !global_stack with
  | [] -> raise (CompilationError "No frame descriptor")
  | (frame::_) ->
    (* assert (frame.rsp_offset > 0); *)
    frame.rsp_offset <- frame.rsp_offset - (value_size * num);
    if debug then 
      Printf.printf "Pop (_, %d) %s\n" frame.rsp_offset msg;
    assert (frame.rsp_offset >= 0)
;;

let pop msg =
  popN 1 msg
;;

let push_local_variable name =
  let offset = push "Place local var" in
  let frame = List.hd (!global_stack) in
  frame.stack_values <- (name, offset)::frame.stack_values;
  if debug then
    Printf.printf "Push loc (%s, %d)\n" name offset
;;

(* Note: it doesn't move rsp_offset *)
let pop_local_variable name =
  let frame = List.hd (!global_stack) in
  frame.stack_values <- List.filter (* TODO: make O(1) *)
    (fun (n , _) -> n != name) 
    frame.stack_values
;;

let print_values frame =
  List.fold_left (fun _ (name, off) ->
    Printf.printf "Frame (%s, %d)\n" name off
  ) () frame.stack_values

let get_local_var_offset name : int option =
  let frame = List.hd (!global_stack) in
  if debug then
    print_values frame;
  let maybe_offset = List.find_opt (fun (name', _) -> name = name') frame.stack_values in
  match maybe_offset with
  | None -> None
  | Some (_, off) -> Some off
;;

let get_arg_place name : int_register option =
  let rec find_arg arg args counter : int option =
    match args with
    | [] -> None
    | h :: tl -> if arg = h then (Some counter) else find_arg arg tl (counter+1)
  in

  let args = (List.hd !global_stack).args in
  match (find_arg name args 0) with
  | None -> None
  | Some id -> Some (List.nth regs_for_int_arguments id)
;;

(* 
  Gets place of local variable. It can be both regular local binding
  or function argument. Thus there are two kinds of places that 
  can be returned from the function:
    - Memory ptr to stack slot (for local bindings)
    - Integer register (for arguments)
*)
let get_local_var_place name : operand = 
  if debug then
    Printf.printf "Get loc %s\n" name;
  let maybe_on_stack = get_local_var_offset name in
  match maybe_on_stack with
  | Some off -> Op_mem_ptr (FromPlaceSubOff (`RBP, off))
  | None ->
    let maybe_in_reg = get_arg_place name in
    match maybe_in_reg with
    | Some reg -> Op_reg reg
    | None -> Context.log_func_map(); raise (CompilationError ("Variable " ^ name ^ " is undefined"))
;;

let alloc_label_id () =
  let old = !label_counter in
  label_counter := !label_counter + 1;
  old
;;