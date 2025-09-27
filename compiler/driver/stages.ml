open Shared.Ir

module Stage = struct
  type t = {
    name:         string;
    action:       (ir_kind -> ir_kind);
    is_printable: bool
  }

  let constr name action = {
    name = name; 
    action = action; 
    is_printable = true
  }

  let constr_hidden name action = {
    name = name; 
    action = action; 
    is_printable = false
  }

  let execute stage input =
    if stage.is_printable then
      Printf.printf "\nStage: %s\n" stage.name;

    stage.action input

end


let stages_executor (data : ir_kind) (stages : Stage.t array): ir_kind =
  let rec stages_executor' (stages: Stage.t array) (i: int) (data: ir_kind) =
    if i >= Array.length stages then
      data (* End of execution *)
    else

    let current_stage = stages.(i) in
    let result = Stage.execute current_stage data in
    let next_i = if current_stage.name = "repeat" then 0 else i + 1 in

    stages_executor' stages next_i result
  in

  stages_executor' stages 0 data


let stages_executor_no_input (stages : Stage.t array): ir_kind =
  stages_executor `Nothing stages