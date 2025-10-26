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
  
  let show_node_kind = function
  | ListLiteral -> "Literal.List"
  | Ident       -> "Identifier"
  | Let         -> "Exp.Let"
  | If          -> "Exp.If"
  | Cond        -> "Exp.Cond"
  | Call        -> "Exp.Call"
  | Lambda      -> "Exp.Lambda"
  | Literal     -> "Literal"

  type t = {
    kind:          node_kind;
    name:          string option;
    args:          t list;
    value:         value;
    line:          int;
    mutable ntype: Type.t option;
  }

  let node kind ?(name = None) ?(args = []) ?(value = NoValue) ?(line = 0) ?(ntype = None) (): t =
    Log.logMsg Log.PARSER @@ Format.sprintf "New node %s" (show_node_kind kind);
    { kind = kind;
      name = name;
      args = args;
      value = value;
      line = line;
      ntype = ntype; }

  let identifier (name: string) ?(line = 0) (): t =
    node Ident ~name:(Some name) ~line:line ()

  let literal (v: value) ?(line = 0) (): t =
    node Literal ~value:v ~line:line ()

  (** Includes subtree *)
  let show (node: t): string =
    let rec show0 n lvl =

      let {kind; name; args; value; line; ntype} = n in
      let tab = Utils.string_rep "│\t" lvl in
      let header = Format.sprintf "├─%s %sval[%s] type[%s] ln[%s]\n"
        (show_node_kind kind)
        (if Option.is_some name then Format.sprintf "name[%s] " (Option.get name) else "")
        (if value <> NoValue then show_value value else "")
        (if Option.is_some ntype then Option.get ntype |> Type.show else "")
        (string_of_int line)
      in
      let sargs = List.map (fun arg -> show0 arg (lvl + 1)) args in
      tab ^ header ^ (String.concat "" sargs)
    in
    show0 node 1

end

type def_kind =
  | Const
  | TypeDef
  | FuncDef
  | FuncDecl
  [@@deriving show]

type def_desc = {
  kind:              def_kind;
  name:              string;
  args:              string list;
  line:              int;
  mutable def_type:  Type.t option;
  mutable body_tree: Node.t option;
}

let show_def_desc (dd: def_desc): string =
  let header = Format.sprintf "\n\n%s name[%s] line[%s] type[%s]\n" 
    (show_def_kind dd.kind)
    dd.name
    (string_of_int dd.line)
    (if Option.is_some dd.def_type then Option.get dd.def_type |> Type.show else "")
  in
  let sargs = Format.sprintf "args[%s]\n" @@ String.concat " " dd.args in
  let body0 = match dd.body_tree with 
    | None -> ""
    | Some node -> Node.show node
  in
  let body = Format.sprintf "body[\n%s]\n" body0 in
  header ^ sargs ^ body


type module_desc = {
  name:    string;
  imports: string list;
  exports: string list;
  defs:    def_desc list;
}

let show_module_desc (md: module_desc): string =
  Format.sprintf "module name[%s]\ndefs[%s\n]" 
    md.name
    (String.concat "" @@ List.map show_def_desc md.defs)


type stage_data =
  | Nothing
  | Module  of module_desc  
  | String  of string
  | Channel of in_channel


exception InvalidIRKind of string
