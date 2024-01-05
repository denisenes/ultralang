open Shared.Common

open Common

type fm = (name * instruction list * bool) list

let func_map : fm ref = ref []

let add_func name body is_global : unit = 
  func_map := (name, body, is_global) :: !func_map

let log_func_map () : unit =
  Printf.printf "Functions:\n";
  List.fold_left (fun _ (name, _, is_global) -> 
    Printf.printf "\t[%s]: is global = %s\n" name (Shared.Utility.pretty_bool is_global)
  ) () !func_map