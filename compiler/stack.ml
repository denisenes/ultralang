open Common

type stack = {
  mutable off : int; (* rsp = rbp + off *)
  mutable values : (string * int) list
}

let global_stack : stack = { off=0; values=[] }

let push_spilled_val () =
  let old_sp = global_stack.off in
  global_stack.off <- global_stack.off + value_size;
  old_sp;;

let pop_spilled_val () =
  global_stack.off <- global_stack.off - value_size;
  ();;