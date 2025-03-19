open Ast
open Symbol
module M = Monotype.Ast

let free_vars e =
  let rec go (t, e) =
    match e with
    | M.Var x -> SymbolMap.singleton x t
    | M.Int _ -> SymbolMap.empty
    | M.Str _ -> SymbolMap.empty
    | M.Tag (_, es) -> List.map go es |> SymbolMap.concat
    | M.Record fields -> List.map snd fields |> List.map go |> SymbolMap.concat
    | M.Access (e, _) -> go e
    | M.Let (`Letval (Letval { bind = _, x; body }), rest) ->
        let free_body = go body in
        let free_rest = go rest |> SymbolMap.remove x in
        SymbolMap.union free_body free_rest
    | M.Let
        (`Letfn (Letfn { bind = _, x; arg = _, a; body; recursive = _ }), rest)
      ->
        let free_body = go body |> SymbolMap.remove x |> SymbolMap.remove a in
        let free_rest = go rest |> SymbolMap.remove x in
        SymbolMap.union free_body free_rest
    | M.Clos { arg = _, x; body } -> go body |> SymbolMap.remove x
    | M.Call (e1, e2) -> SymbolMap.union (go e1) (go e2)
    | M.KCall (_, es) -> List.map go es |> SymbolMap.concat
    | M.When (e, bs) ->
        let free_e = go e in
        let free_bs = List.map go_branch bs |> SymbolMap.concat in
        SymbolMap.union free_e free_bs
  and go_branch (p, e) =
    let bound_p = go_pat p in
    let free_e = go e in
    SymbolMap.diff free_e bound_p
  and go_pat (t, p) =
    match p with
    | M.PTag (_, ps) -> List.map go_pat ps |> SymbolMap.concat
    | M.PVar x -> SymbolMap.singleton x t
  in
  go e

let flip (x, y) = (y, x)
let lookup_new_symbol x venv = List.assoc_opt x venv |> Option.value ~default:x

let lambda_lift_expr ~(ctx : Ctx.t) expr : def list * e_expr =
  let rec go venv (t, e) =
    let lifted, e' =
      match e with
      | M.Var x ->
          let x = lookup_new_symbol x venv in
          ([], Var x)
      | M.Int x -> ([], Int x)
      | M.Str x -> ([], Str x)
      | M.Tag (x, es) ->
          let lifted, es = List.split (List.map (go venv) es) in
          (List.flatten lifted, Tag (x, es))
      | M.Record fields ->
          let go_field (field, e) =
            let lifted, e = go venv e in
            (lifted, (field, e))
          in
          let lifted, fields = List.split (List.map go_field fields) in
          (List.flatten lifted, Record fields)
      | M.Access (e, field) ->
          let lifted, e = go venv e in
          (lifted, Access (e, field))
      | M.Let (`Letval (Letval { bind; body }), rest) ->
          let lifted1, body = go venv body in
          let lifted2, rest = go venv rest in
          let expr = Let (bind, body, rest) in
          (lifted1 @ lifted2, expr)
      | M.Let (`Letfn (Letfn { bind = t_x, x; arg; body; recursive = _ }), rest)
        ->
          let captures =
            free_vars body |> SymbolMap.bindings
            |> List.map (fun (x, t) -> (lookup_new_symbol x venv, t))
            |> List.to_seq |> SymbolMap.of_seq
            |> SymbolMap.remove_keys [ snd arg; x ]
            |> SymbolMap.remove_keys !(ctx.toplevels)
            |> SymbolMap.bindings |> List.map flip
          in
          (* A rename is needed to avoid conflicts with the same symbol in a
             different letfn that needs to be lifted. *)
          let x' =
            ctx.symbols.fresh_symbol_named (Symbol.syn_of ctx.symbols x)
          in
          if List.length captures = 0 then (
            Ctx.add_toplevel ctx x';
            let lifted1, body = go ((x, x') :: venv) body in
            let def = ((t_x, x'), `Fn { arg; captures; body }) in
            let lifted2, (_, rest) = go ((x, x') :: venv) rest in
            (lifted1 @ lifted2 @ [ def ], rest))
          else
            let let_x =
              ctx.symbols.fresh_symbol_named (Symbol.syn_of ctx.symbols x)
            in
            let lifted1, body = go ((x, x') :: venv) body in
            let def = ((t_x, x'), `Fn { arg; captures; body }) in
            let lifted2, rest = go ((x, let_x) :: venv) rest in
            let let_x = Let ((t_x, let_x), (t_x, Var x'), rest) in
            (lifted1 @ lifted2 @ [ def ], let_x)
      | M.Clos { arg; body } ->
          let captures =
            free_vars body |> SymbolMap.bindings
            |> List.map (fun (x, t) -> (lookup_new_symbol x venv, t))
            |> List.to_seq |> SymbolMap.of_seq
            |> SymbolMap.remove (snd arg)
            |> SymbolMap.remove_keys !(ctx.toplevels)
            |> SymbolMap.bindings |> List.map flip
          in
          let lifted, body = go venv body in
          let clos_name = ctx.symbols.fresh_symbol_named "clos" in
          let def = ((t, clos_name), `Fn { arg; captures; body }) in
          (lifted @ [ def ], Var clos_name)
      | M.Call (e1, e2) ->
          let lifted1, e1 = go venv e1 in
          let lifted2, e2 = go venv e2 in
          (lifted1 @ lifted2, Call (e1, e2))
      | M.KCall (f, es) ->
          let lifted, es = List.split (List.map (go venv) es) in
          (List.flatten lifted, KCall (f, es))
      | M.When (e, bs) ->
          let lifted1, e = go venv e in
          let lifted2, bs = List.split (List.map (go_branch venv) bs) in
          (lifted1 @ List.flatten lifted2, When (e, bs))
    in
    (lifted, (t, e'))
  and go_branch venv (p, e) =
    let lifted, e = go venv e in
    let p = go_pat venv p in
    (lifted, (p, e))
  and go_pat venv (t, p) =
    let p' =
      match p with
      | M.PTag (x, ps) ->
          let ps = List.map (go_pat venv) ps in
          PTag (x, ps)
      | M.PVar x -> PVar x
    in
    (t, p')
  in
  go [] expr

let lambda_lift_letfn ~ctx (Letfn { bind; arg; body; recursive = _ } : M.letfn)
    : def list =
  let lifted, body = lambda_lift_expr ~ctx body in
  let def = (bind, `Fn { arg; captures = []; body }) in
  lifted @ [ def ]

let lambda_lift_letval ~ctx (Letval { bind; body } : M.letval) : def list =
  let lifted, body = lambda_lift_expr ~ctx body in
  let def = (bind, `Val body) in
  lifted @ [ def ]

let lambda_lift_run ~ctx (Run { bind; body; ty } : M.run_def) : def list =
  let lifted, body = lambda_lift_expr ~ctx body in
  let def = (bind, `Run (body, ty)) in
  lifted @ [ def ]

let lambda_lift_def ~ctx = function
  | `Def (`Letfn letfn) -> lambda_lift_letfn ~ctx letfn
  | `Def (`Letval letval) -> lambda_lift_letval ~ctx letval
  | `Run run -> lambda_lift_run ~ctx run

let lower : Ctx.t -> M.program -> program =
 fun ctx program ->
  let defs = List.flatten @@ List.map (lambda_lift_def ~ctx) program in
  defs
