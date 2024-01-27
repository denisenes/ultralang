open Shared.Common

open Common

module NameSpace = Set.Make(String)

type fm = (name * instruction list * bool) list

(* Contains metainformation about already processed functions *)
let func_map   : fm ref          = ref []
(* Set for fast checking if identifier is a function name *)
let func_names : NameSpace.t ref = ref (NameSpace.empty)

let add_func name body is_global : unit = 
  func_map   := (name, body, is_global) :: !func_map;
  func_names := !func_names |> NameSpace.add name

let lookup_name name =
  !func_names |> NameSpace.mem name

let log_func_map () : unit =
  Printf.printf "Functions:\n";
  List.fold_left (fun _ (name, _, is_global) -> 
    Printf.printf "\t[%s]: is global = %s\n" name (Shared.Utility.pretty_bool is_global)
  ) () !func_map