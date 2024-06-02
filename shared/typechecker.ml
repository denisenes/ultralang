open Common
open Printer
(* open Utility.ExtSyntax *)

(* term variable -> type scheme *)
module Context = Map.Make(String)
(* type variable -> type *)
module Substs  = Map.Make(String)

let debug = false

let rec type_to_string = 
  let args_to_string args =
    List.fold_left (fun acc arg ->
      Printf.sprintf "%s %s" acc (type_to_string arg)
    ) "" args |> String.trim
  in
  function
  | `TyBool   -> "Bool"
  | `TyInt    -> "Int"
  | `TyList a -> Printf.sprintf "[%s]" @@ type_to_string a
  | `TyFun (args, retpe) -> 
    Printf.sprintf "([%s] -> %s)" (args_to_string args) (type_to_string retpe)
  | `TyVar a  -> a

let subst_to_string sbsts =
  "[" ^
    List.fold_left (fun acc (v, tpe) ->
      Printf.sprintf "%s (%s -> %s)" acc v (type_to_string tpe))
      "" (Substs.to_list sbsts)
  ^ "]"

let var_counter = ref 0

let new_ty_var () : ty =
  let res = Printf.sprintf "T%d" !var_counter in
  var_counter := !var_counter + 1;
  `TyVar res

(** Apply substitution to type *)
let rec ( *> ) (subst : ty Substs.t) (tpe : ty) : ty = 
  match tpe with
  | `TyBool | `TyInt -> tpe
  | `TyList t -> subst *> t
  | `TyFun (args, body) -> `TyFun (
      List.map (fun arg -> subst *> arg) args,
      subst *> body
    )
  | `TyVar var -> Utility.from_opt (`TyVar var) (Substs.find_opt var subst)


(** Apply substitution to scheme *)
let ( *>> ) (subst : ty Substs.t) (schm : ty_scheme) : ty_scheme =
  let Scheme (quant_vars, tpe) = schm in
  (* Do not substitute to quantified vars *)
  let subst' = Substs.filter (fun v _ -> not @@ List.mem v quant_vars) subst in
  let new_tpe = subst' *> tpe in
  Scheme (quant_vars, new_tpe)

(** Apply substitution to context *)
let app_subst_to_ctx (subst : ty Substs.t) (ctx : ty_scheme Context.t) =
  Context.map (fun scm -> subst *>> scm) ctx 

(** Composition of substitutions *)
let ( <*> ) s1 s2 =
  Substs.union (fun _ _ t -> Some t)
    s1 (Substs.map (fun t -> s1 *> t) s2)

let compose_subs subs =
  List.fold_right (fun x acc -> acc <*> x) subs Substs.empty

let rec free_vars (tpe : ty) =
    match tpe with
    | `TyBool | `TyInt -> []
    | `TyList t -> free_vars t
    | `TyFun (args, body) -> 
      let args_fv = (List.fold_left (fun acc a -> (free_vars a) @ acc) [] args) in
      (free_vars body) @ args_fv
    | `TyVar var -> [var]

let bind_var (var : name) (tpe : ty) : ty Substs.t =
  match var with
  | _ when tpe = `TyVar var -> Substs.empty
  | _ when List.mem var (free_vars tpe) -> 
    raise @@ TypeCheckError (Printf.sprintf "Occurs check error: %s in %s" var (type_to_string tpe))
  | _ -> Substs.singleton var tpe
  (* cond [
    case(tpe = `TyVar var)             => Substs.empty;
    case(List.mem var (free_vars tpe)) =>
      raise @@ TypeCheckError (Printf.sprintf "Occurs check error: %s in %s" var (type_to_string tpe));
    otherwise                          => Substs.singleton var tpe;
  ] *)

let rec unify (tpe1 : ty) (tpe2 : ty) : ty Substs.t =
  match (tpe1, tpe2) with
  | (`TyBool, `TyBool) -> Substs.empty
  | (`TyInt , `TyInt ) -> Substs.empty
  | (`TyVar name , _ ) -> bind_var name tpe2
  | ( _ , `TyVar name) -> bind_var name tpe1
  | (`TyList t1, `TyList t2) -> unify t1 t2
  | (`TyFun (args1, b1), `TyFun (args2, b2))
    when List.length args1 = List.length args2 ->
    let args_sub = List.fold_left2 (fun acc a1 a2 -> acc <*> (unify a1 a2))
      Substs.empty args1 args2
    in args_sub <*> (unify b1 b2) 
  | _ -> 
    let err = Printf.sprintf "Can't unify types: t1=%s with t2=%s" 
      (type_to_string tpe1)
      (type_to_string tpe2) in
    raise @@ TypeCheckError err

let instantiate (schm : ty_scheme) : ty =
  let Scheme (vars, tpe) = schm in
  let new_tvs = List.map (fun _ -> new_ty_var()) vars    in 
  let subst = Substs.of_list (List.combine vars new_tvs) in
  subst *> tpe

let rec infer_call ctx fname args : ty * ty Substs.t =
  let template = match spec_binding_ty fname with
    | Some scm -> scm
    | None     -> Context.find fname ctx
  in
  let instance = instantiate template in
  let args_inf = List.map (fun x -> infer ctx x) args in
  let (a_tps, a_sbs) = List.split args_inf in
  let ret_tpe  = new_ty_var () in
  let app_tpe  = `TyFun (a_tps, ret_tpe) in
  let sbs      = unify app_tpe instance in
  let sbs'     = compose_subs @@ sbs :: a_sbs in
  (sbs' *> ret_tpe, sbs')


and infer_func_def ctx args body : ty * ty Substs.t =
  (* Prepare new type vars for arguments *)
  let vars = List.map (fun _ -> new_ty_var()) args in
  (* Enrich context with arguments *)
  let ctx' = List.fold_left2
    (fun acc v a -> Context.add a (Scheme ([], v)) acc) ctx vars args
  in
  (* Infer body type *)
  let (body_ty, s) = infer ctx' body in
  (* Apply substitutions to argument type vars *)
  let vars' = List.map (fun t -> s *> t) vars in
  (* Ret *)
  (`TyFun (vars', body_ty), s)

and infer_if ctx b_exp t_exp f_exp : ty * ty Substs.t =
  (* Infer all subexpressions and update context *)
  let (b_tpe, b_subst) = infer ctx b_exp in
  let ctx = app_subst_to_ctx b_subst ctx in
  let (t_tpe, t_subst) = infer ctx t_exp in
  let ctx = app_subst_to_ctx t_subst ctx in
  let (f_tpe, f_subst) = infer ctx f_exp in
  let _   = app_subst_to_ctx f_subst ctx in
  (* Combine all substitutions *)
  let subst = (b_subst <*> t_subst) <*> f_subst in
  (* Update types by all substitutions *)
  let b_tpe = subst *> b_tpe in
  let t_tpe = subst *> t_tpe in
  let f_tpe = subst *> f_tpe in
  (* Unify conditional exp with Bool *)
  let unif_subst1 = unify b_tpe `TyBool in
  (* Check branches have the same type *)
  let unif_subst2 = unify t_tpe f_tpe in
  (* Combine all substitutions *)
  let res_subst = (subst <*> unif_subst1) <*> unif_subst2 in
  let res_tpe   = res_subst *> t_tpe in
  (res_tpe, res_subst)

and infer ctx exp : ty * ty Substs.t =
  let (tpe, substs) = 
    match exp with
    | Literal (Fixnum _)   -> (`TyInt,  Substs.empty)
    | Literal (Boolean _)  -> (`TyBool, Substs.empty)
    | Ident var            -> (
      match Context.find_opt var ctx with
      | None -> raise @@ TypeCheckError ("Unbound variable " ^ var)
      | Some schm -> 
        let tpe = instantiate schm in
        (tpe, Substs.empty)
    )
    | Lambda (args, body)  -> infer_func_def ctx args body
    | Call (fname, args)   -> infer_call ctx fname args
    | Let (var, exp, body) ->
      let (tpe1, s1) = infer ctx exp in
      let ctx' = Context.add var (Scheme ([], tpe1)) ctx in
      let (tpe2, s2) = infer ctx' body in
      (tpe2, s2 <*> s1)
    | If (b_exp, t_exp, f_exp) -> infer_if ctx b_exp t_exp f_exp
    | _ -> raise @@ TypeCheckError ("No supported yet")
  in
  if debug then
    Printf.printf "%s : %s with %s\n" 
      (exp_to_string exp 0) 
      (type_to_string tpe)
      (subst_to_string substs);
  (tpe, substs)

let generalize _ tpe : ty_scheme =
  Scheme (free_vars tpe, tpe)

let infer_hl ctx hl_entry : (ty * ty Substs.t) * ty_scheme Context.t = 
  match hl_entry with
  | HLExp exp -> (infer ctx exp, ctx)
  | DefFn (fname, args, body) ->
    (* Construct scheme with fresh type variables *)
    let tpe = `TyFun (List.map (fun _ -> new_ty_var()) args, new_ty_var()) in
    let fresh_scheme = Scheme (free_vars tpe, tpe) in
    (* Enrich context with fresh scheme (for recursion) and infer *)
    let ctx' = Context.add fname fresh_scheme ctx in 
    let (tpe, substs) = infer_func_def ctx' args body in
    (* Generalize final func type and add to context *)
    let res_scheme = generalize ctx tpe in
    ((tpe, substs), Context.add fname res_scheme ctx)
  | _ -> raise (TypeCheckError "Not supported yet")

let infer_one_hl hl_entry : ty * ty_scheme Context.t =
  let ctx_init = (Context.empty : ty_scheme Context.t) in
  let ((tpe, _), ctx) = infer_hl ctx_init hl_entry in
  (tpe, ctx)

let infer_program (p : program) =
  let ctx_init = (Context.empty : ty_scheme Context.t) in
  List.fold_left (fun (tpes, ctx) hle -> 
    let ((tpe, _), new_ctx) = infer_hl ctx hle in
    (tpes @ [tpe], new_ctx) 
  ) ([], ctx_init) p
  