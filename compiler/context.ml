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
    `GlobalVal                    |
    `GlobalFn of instruction list |
    `External
  ]

type binding_type = [local_binding_type | global_binding_type]

module Context = Map.Make(String)

let context = ref (Context.empty : binding_type Context.t)

let register_func (name : string) (body : instruction list) : unit = 
  context := !context |> Context.add name (`GlobalFn body)

let register_local (name : string) : unit =
  context := !context |> Context.add name `Local

let register_arg (name : string) : unit =
  context := !context |> Context.add name `Argument

let init_context () =
  List.fold_left (fun _ name -> 
    context := !context |> Context.add name `External
  ) () 
  runtime_bindings

let get_binding_type name : binding_type =
  try
    !context |> Context.find name
  with
    Not_found -> raise (Shared.Common.CompilationError ( 
      "Couldn't find binding: " ^ name))

let log_func_map () : unit =
  Printf.printf "Global functions:\n";
  Context.fold (fun name binding _ ->
    match binding with
    | `GlobalFn _ -> Printf.printf "\t[%s]\n" name
    | _ -> ()
  ) !context ()