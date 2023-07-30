open Common

let mkloc () = ref None

let bindloc (name, value_or_none, env) : value env = 
  (name, value_or_none)::env

let rec lookup = function
  | (name, []) -> raise (NotFoundInEnv ("Couldn't find value for name " ^ name))
  | (name, (name', value_or_none)::rest) ->
    if name = name' then match !value_or_none with
      | Some value -> value
      | None -> raise (ValueNotSpecified ("Value for name " ^ name ^ "doesn't exist"))
    else lookup (name, rest)

let bind (name, value, env) =
  (name, ref (Some value))::env

let bind_bunch names values env =
  List.fold_left2 (fun acc name value -> bind (name, value, acc)) env names values