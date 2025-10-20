type utype = Type of string (* TODO *)

module Node = struct

  type value =
    | NoValue
    | Str of string
    | Int of int
    | Bool of bool (* TODO remove and represent as ADT *)
    | Nil
    | Unit
  [@@deriving show]

  type node_kind =
    (* Sugar *)
    | ListLiteral
    (* Core nodes *)
    | Ident
    | Let
    | If
    | Cond
    | Call
    | Lambda
    | Literal
  [@@deriving show]

  type t = {
    kind:          node_kind;
    args:          t list;
    value:         value;
    line:          int;
    mutable ntype: utype option;
  }

  let node kind ?(args = []) ?(value = NoValue) ?(line = 0) ?(ntype = None) (): t =
    { kind = kind;
      args = args;
      value = value;
      line = line;
      ntype = ntype; }

  let identifier (name: string) ?(line = 0) (): t =
    node Ident ~value:(Str name) ~line:line ()

  let literal (v: value) ?(line = 0) (): t =
    node Literal ~value:v ~line:line ()

  (** Includes subtree *)
  let show (node: t): string =
    let rec show0 n lvl =

      let {kind; args; value; line; _} = n in
      let tab = String.make lvl '\t' in
      let header = Format.sprintf "|-> %s val[%s] type[%s] ln[%s]\n"
        (show_node_kind kind)
        (show_value value)
        "N/A" (* TODO *)
        (string_of_int line)
      in
      let sargs = List.map (fun arg -> show0 arg (lvl + 1)) args in
      tab ^ header ^ (String.concat "" sargs)
    in
    show0 node 0

end

type def_desc = {
  name:              string;
  args:              string list;
  line:              int;
  mutable def_type:  utype option;
  mutable body_tree: Node.t option;
}

let show_def_desc (dd: def_desc): string =
  let header = Format.sprintf "\ndef name[%s] line[%s] type[%s]\n" 
    dd.name
    (string_of_int dd.line)
    "N/A" (* TODO *)
  in
  let sargs = Format.sprintf "args[%s]\n" @@ String.concat " " dd.args in
  let body0 = match dd.body_tree with 
    | None -> ""
    | Some node -> Node.show node
  in
  let body = Format.sprintf "body[%s]\n" body0 in
  header ^ sargs ^ body


type module_desc = {
  name:    string;
  imports: string list;
  exports: string list;
  defs:    def_desc list;
}

let show_module_desc (md: module_desc): string =
  Format.sprintf "module name[%s]\ndefs[%s]" 
    md.name
    (String.concat "" @@ List.map show_def_desc md.defs)


type stage_data =
  | Nothing
  | Module  of module_desc  
  | String  of string
  | Channel of in_channel


exception InvalidIRKind of string
