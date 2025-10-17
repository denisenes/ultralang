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
    let {kind; args; value; line; _} = node in
    let header = Format.sprintf "%s val[%s] tpe[%s] ln[%s]"
      (show_node_kind kind)
      (show_value value)
      "N/A" (* TODO *)
      (string_of_int line)
    in
    (* TODO args *)

end

type def_desc = {
  name:              string;
  args:              string list;
  line:              int;
  mutable def_type:  utype option;
  mutable body_tree: Node.t option;
}


type module_desc = {
  name:    string;
  imports: string list;
  exports: string list;
  defs:    def_desc list;
}


type stage_data =
  | Nothing
  | Module  of module_desc  
  | String  of string
  | Channel of in_channel


exception InvalidIRKind of string