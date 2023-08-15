open Common

type stack = {
  mutable offset : int; (* rsp = rbp + off *)
  mutable values : (string * int) list
  (* TODO: introduce function frames abstraction *)
}

let global_stack : stack = { offset=0; values=[] }

let push () =
  global_stack.offset <- global_stack.offset + value_size;
  global_stack.offset;;

let pop () =
  global_stack.offset <- global_stack.offset - value_size;
  ();;

let push_local_variable name =
  let index = push() in
  global_stack.values <- (name, index)::global_stack.values

let get_local_var_offset name : int option =
  let maybe_offset = List.find_opt (fun (name', _) -> name = name') global_stack.values in
  match maybe_offset with
  | None -> None
  | Some (_, off) -> Some off