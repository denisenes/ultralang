type t = 
  | Var  of string           (* e.g. $A, $B *)
  | Atom of string           (* e.g. Int, Bool *)
  | App  of string * t list  (* e.g. List[Int], Fn[$A, $B] *)


let rec show: t -> string = function
  | Var v  -> "%" ^ v
  | Atom a -> a
  | App (name, args) -> 
    Format.sprintf "%s[%s]" name (String.concat "," @@ List.map show args)


type variant = {
  vname:     string;
  vargs:     string list;
  vtemplate: t;
}


type adt_info = {
  name:     string;
  template: t;
  variants: variant list
}


module AdtRegistry = struct 
  
  module AdtMap = Map.Make(String)

  let registry: (adt_info AdtMap.t) ref = ref AdtMap.empty

  let add (adt: adt_info): unit =
    let name = adt.name in
    registry := (AdtMap.add name adt !registry)

  let find (name: string): adt_info option =
    AdtMap.find_opt name !registry

  let show(): string =
    let header = "\n\nADTs:\n" in
    let str_adts' = AdtMap.map (fun adt ->
      Format.sprintf "%s %s"
        adt.name
        (show adt.template)
    ) !registry in
    let str_adts = AdtMap.to_list str_adts' |> List.split |> fst in
    header ^ (String.concat "\n" str_adts)

end