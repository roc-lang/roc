open Ast
open Ctx
open Lower_type
open Layout
module M = Lambdamono.Ast
module T = Lambdamono.Type
module P = Lambdamono.Type_print

let index_of f =
  let rec go i = function
    | [] -> failwith "index_of"
    | x :: xs -> if f x then i else go (i + 1) xs
  in
  go 0

let tag_id : T.tvar -> string -> int =
 fun ty ctor ->
  match T.tvar_deref ty with
  | T.TTag tags -> index_of (fun (name, _) -> name = ctor) tags
  | _ -> failwith "not a tag"

let field_id : T.tvar -> string -> int =
 fun ty field ->
  match T.tvar_deref ty with
  | T.TRecord fields -> index_of (fun (name, _) -> name = field) fields
  | _ -> failwith ("not a record: " ^ P.show_ty ty)

let get_pat_var : ctx:Ctx.t -> M.e_pat -> var =
 fun ~ctx pat ->
  match pat with
  | t, PVar v -> (lower_type ctx.type_cache t, v)
  | _ -> failwith "non-var pattern not yet supported"

let lower_expr ~ctx e : stmt list * var =
  let lower_type = lower_type ctx.type_cache in
  let rec go_var (t, e) : stmt list * var =
    let asgns, expr = go_expr (t, e) in
    let l = lower_type t in
    match expr with
    | Var var -> (asgns, var)
    | _ ->
        let var = (l, ctx.symbols.fresh_symbol "var") in
        let asgns = asgns @ [ Let (var, expr) ] in
        (asgns, var)
  and go_expr (t, e) : stmt list * expr =
    let l = lower_type t in
    match e with
    | M.Var x -> ([], Var (l, x))
    | M.Int i -> ([], Lit (`Int i))
    | M.Str s -> ([], Lit (`String s))
    | M.Unit -> ([], MakeStruct [])
    | M.Tag (ctor, args) ->
        let struct_layout =
          ref @@ Struct (List.map fst args |> List.map lower_type)
        in
        let struct_var = (struct_layout, ctx.symbols.fresh_symbol "struct") in
        let stmts, args = List.split @@ List.map go_var args in
        let let_struct = Let (struct_var, MakeStruct args) in
        let union = MakeUnion (tag_id t ctor, struct_var) in
        (List.concat stmts @ [ let_struct ], union)
    | M.Record fields ->
        let stmts, fields =
          List.split @@ List.map go_var @@ List.map snd fields
        in
        (List.concat stmts, MakeStruct fields)
    | M.Access (e, field) ->
        let field = field_id (fst e) field in
        let stmts, e = go_var e in
        let access = GetStructField (e, field) in
        (stmts, access)
    | M.Let ((t_x, x), body, rest) ->
        let var = (lower_type t_x, x) in
        let stmts_body, body = go_expr body in
        let stmts_rest, rest = go_expr rest in
        let stmts = stmts_body @ [ Let (var, body) ] @ stmts_rest in
        (stmts, rest)
    | M.Call (f, args) ->
        let stmts, args = List.split @@ List.map go_var args in
        let call = CallDirect (f, args) in
        (List.concat stmts, call)
    | M.PackedFn { lambda; captures } ->
        let fn_var = (ref OpaquePtr, ctx.symbols.fresh_symbol "fn") in
        let lambda = FnPtr lambda in
        let let_fn_var = Let (fn_var, lambda) in
        let captures_stmts, captures_var =
          match captures with
          | None ->
              let captures_var =
                (ref OpaquePtr, ctx.symbols.fresh_symbol "captures")
              in
              let captures_struct = NullPtr in
              let let_captures = Let (captures_var, captures_struct) in
              ([ let_captures ], captures_var)
          | Some captures ->
              let stmts, captures = go_var captures in
              (stmts, captures)
        in
        let call = MakeStruct [ fn_var; captures_var ] in
        ([ let_fn_var ] @ captures_stmts, call)
    | M.CallIndirect (f, args) ->
        let stmts_f, f = go_var f in
        let stmts_args, args = List.split @@ List.map go_var args in
        let call = CallIndirect (f, args) in
        (stmts_f @ List.concat stmts_args, call)
    | M.KCall (kfn, args) ->
        let stmts, args = List.split @@ List.map go_var args in
        let call = CallKFn (kfn, args) in
        (List.concat stmts, call)
    | M.When (tag, branches) ->
        let tag_stmts, tag_var = go_var tag in
        let discr_var = (ref Int, ctx.symbols.fresh_symbol "discr") in
        let discr_asgn = Let (discr_var, GetUnionId tag_var) in
        let branches =
          List.sort (fun (tag_id1, _) (tag_id2, _) -> tag_id1 - tag_id2)
          @@ List.map (go_branch tag_var) branches
        in
        let join = (l, ctx.symbols.fresh_symbol "join") in
        let switch = Switch { cond = discr_var; branches; join } in
        let stmts = tag_stmts @ [ discr_asgn ] @ [ switch ] in
        (stmts, Var join)
  and go_branch : var -> M.branch -> int * (stmt list * expr) =
   fun tag_var (pat, body) ->
    match pat with
    | p_ty, PTag (tag, args) ->
        let tag_id = tag_id p_ty tag in
        let stmts =
          if args = [] then []
          else
            let arg_vars = List.map (get_pat_var ~ctx) args in
            let struct_layout = ref @@ Struct (List.map fst arg_vars) in
            let tag_payload_var =
              (struct_layout, ctx.symbols.fresh_symbol "payload")
            in
            let tag_payload_stmts =
              Let (tag_payload_var, GetUnionStruct tag_var)
            in
            let arg_destructs =
              List.mapi
                (fun i var -> Let (var, GetStructField (tag_payload_var, i)))
                arg_vars
            in
            tag_payload_stmts :: arg_destructs
        in
        let body_asgns, body_expr = go_expr body in
        let asgns = stmts @ body_asgns in
        (tag_id, (asgns, body_expr))
    | _ -> failwith "non-tag pattern not yet supported"
  in
  go_var e

let lower_fn ~ctx name ({ args; body } : M.fn) : def =
  let args : var list =
    List.map (fun (t, x) -> (lower_type ctx.type_cache t, x)) args
  in
  let body, ret = lower_expr ~ctx body in
  Fn { name; args; body; ret }

let lower_val ~ctx ~entry_ty name e =
  let thunk_name =
    ctx.symbols.fresh_symbol_named (Symbol.show_symbol_raw name ^ "_thunk")
  in
  let fn = lower_fn ~ctx thunk_name { args = []; body = e } in
  let layout = lower_type ctx.type_cache (fst e) in
  let global =
    Global { name; layout; init = CallDirect (thunk_name, []); entry_ty }
  in
  [ fn; global ]

let lower_def ~ctx (name, def) : def list =
  match def with
  | `Fn ({ args; body } : M.fn) ->
      let fn = lower_fn ~ctx name { args; body } in
      [ fn ]
  | `Val e -> lower_val ~ctx ~entry_ty:None name e
  | `Run (e, ty) -> lower_val ~ctx ~entry_ty:(Some ty) name e

let lower ~ctx (defs : M.program) : program =
  let defs = List.concat @@ List.map (lower_def ~ctx) defs in
  defs
