open Common

let rec lookup (name, env) =
  match env with
    | Pair(Pair(Symbol name', value), rest) ->
      if name = name' then value
      else lookup (name, rest)
    | _ -> raise (NotFoundInEnv("Couldn't find value for name " ^ name))

let bind (name, value, env) =
  Pair(Pair(name, value), env)