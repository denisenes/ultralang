open Common

(* Binding types are defined by low-level implementation
 * Don't forget that every variant has its own way of getting 
 * the value in native code
 *)

type local_binding_type  =
  [
    `Local    |
    `Argument
  ]

type global_binding_type =
  [
    `GlobalVal |
    `GlobalFn of instruction list
  ]

type binding_type = [local_binding_type | global_binding_type]

module Context = Map.Make(String)

let context = ref (Context.empty : binding_type Context.t)

let add_func (name : string) (body : instruction list) : unit = 
  context := !context |> Context.add name (`GlobalFn body)

let get_binding_type name : binding_type =
  !context |> Context.find name

let log_func_map () : unit =
  Printf.printf "Global functions:\n";
  Context.fold (fun name binding _ ->
    match binding with
    | `GlobalFn _ -> Printf.printf "\t[%s]\n" name
    | _ -> ()
  ) !context ()