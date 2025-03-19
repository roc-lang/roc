open Ast
open Type
open Symbol
module M = Monotype_lifted.Ast
module T = Monotype_lifted.Type

type cache = (T.ty * tvar) list ref

let unfilled_ty = Content (TTag [ ("__lambdasolved_unfilled", []) ])

let inst_type ~(cache : cache) ~fresh_tvar t =
  (*FIXME*)
  let _ = cache in
  let cache = ref [] in
  let rec go (t : T.ty) =
    match List.assq_opt t !cache with
    | Some t -> t
    | None ->
        let t' = fresh_tvar unfilled_ty in
        cache := (t, t') :: !cache;
        let content =
          match !t with
          | T.TFn (t1, t2) -> Content (TFn (go t1, fresh_tvar Unbd, go t2))
          | T.TTag tags ->
              let tags =
                List.map (fun (tag, ts) -> (tag, List.map go ts)) tags
              in
              Content (TTag tags)
          | T.TRecord fields ->
              let fields =
                List.map (fun (field, ty) -> (field, go ty)) fields
              in
              Content (TRecord fields)
          | T.TPrim x -> Content (TPrim x)
        in
        tvar_set t' content;
        t'
  in
  go t

let inst_typed_symbol ~cache ~fresh_tvar (t, x) =
  let t = inst_type ~cache ~fresh_tvar t in
  (t, x)

let inst_expr ~cache ~fresh_tvar (e : M.e_expr) =
  let rec go ((t, e) : M.e_expr) =
    let t = inst_type ~cache ~fresh_tvar t in
    let e =
      match e with
      | M.Var x -> Var x
      | M.Int i -> Int i
      | M.Str s -> Str s
      | M.Tag (tag, es) -> Tag (tag, List.map go es)
      | M.Record fields ->
          let go_field (field, e) = (field, go e) in
          Record (List.map go_field fields)
      | M.Access (e, field) -> Access (go e, field)
      | M.Let (x, e1, e2) ->
          let x = inst_typed_symbol ~cache ~fresh_tvar x in
          Let (x, go e1, go e2)
      | M.Call (e1, e2) -> Call (go e1, go e2)
      | M.KCall (kfn, es) -> KCall (kfn, List.map go es)
      | M.When (e, bs) ->
          let e = go e in
          let bs = List.map go_branch bs in
          When (e, bs)
    in
    (t, e)
  and go_branch (p, e) =
    let p = go_pat p in
    let e = go e in
    (p, e)
  and go_pat ((t, p) : M.e_pat) =
    let t = inst_type ~cache ~fresh_tvar t in
    let p =
      match p with
      | M.PTag (t, ps) -> PTag (t, List.map go_pat ps)
      | M.PVar x -> PVar x
    in
    (t, p)
  in
  go e

let inst_fn ~cache ~fresh_tvar ({ arg; captures; body } : M.fn) =
  let arg = inst_typed_symbol ~cache ~fresh_tvar arg in
  let captures =
    List.map (inst_typed_symbol ~cache ~fresh_tvar) captures
    |> List.map (fun (t, x) -> (x, t))
    |> List.to_seq |> SymbolMap.of_seq
  in
  let body = inst_expr ~cache ~fresh_tvar body in
  { arg; captures; body }

let inst_def ~cache ~fresh_tvar ((x, d) : M.def) : def =
  let x = inst_typed_symbol ~cache ~fresh_tvar x in
  let d =
    match d with
    | `Run (e, t) -> `Run (inst_expr ~cache ~fresh_tvar e, t)
    | `Fn fn -> `Fn (inst_fn ~cache ~fresh_tvar fn)
    | `Val e -> `Val (inst_expr ~cache ~fresh_tvar e)
  in
  (x, d)

let inst : fresh_tvar:fresh_tvar -> Monotype_lifted.Ast.program -> program =
 fun ~fresh_tvar p ->
  let cache = ref [] in
  List.map (inst_def ~cache ~fresh_tvar) p
