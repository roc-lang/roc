open Ast
open Type_clone_inst
open Lower_type
module C = Canonical_solved.Ast
module T = Canonical_solved.Type
module P = Syntax.Type_print

type ctx = {
  symbols : Symbol.t;
  fresh_tvar : T.fresh_tvar;
  specializations : Specializations.t;
}

let specialize_expr ~ctx ~ty_cache ~mono_cache expr =
  let lower_type t =
    lower_type mono_cache @@ clone_inst ctx.fresh_tvar ty_cache t
  in
  let rec go (t, e) =
    let t = clone_inst ctx.fresh_tvar ty_cache t in
    let e =
      match e with
      | C.Var x -> (
          match
            Specializations.specialize_fn ctx.specializations mono_cache x t
          with
          | Some y -> Var y
          | None -> Var x (* No specialization needed *))
      | C.Int i -> Int i
      | C.Str s -> Str s
      | C.Tag (t, args) ->
          let args = List.map go args in
          Tag (t, args)
      | C.Record fields ->
          let go_field (f, e) = (f, go e) in
          let fields = List.map go_field fields in
          Record fields
      | C.Access (e, f) -> Access (go e, f)
      | C.Let (def, rest) ->
          let def =
            match def with
            | `Letfn
                (Letfn
                  { recursive; bind = t_x, x; arg = t_a, a; body; sig_ = _ }) ->
                `Letfn
                  (Letfn
                     {
                       recursive = Option.is_some recursive;
                       bind = (lower_type t_x, x);
                       arg = (lower_type t_a, a);
                       body = go body;
                     })
            | `Letval (Letval { bind = t_x, x; body; sig_ = _ }) ->
                `Letval (Letval { bind = (lower_type t_x, x); body = go body })
          in
          let rest = go rest in
          Let (def, rest)
      | C.Clos { arg = t_arg, arg_sym; body } ->
          let t_arg_new = lower_type @@ clone_inst ctx.fresh_tvar ty_cache t_arg in
          let body = go body in
          Clos { arg = (t_arg_new, arg_sym); body }
      | C.Call (f, a) ->
          let f = go f in
          let a = go a in
          Call (f, a)
      | C.KCall (kfn, args) ->
          let args = List.map go args in
          KCall (kfn, args)
      | C.When (e, branches) ->
          let e = go e in
          let branches = List.map go_branch branches in
          When (e, branches)
    in
    (lower_type t, e)
  and go_branch (p, e) =
    let p = go_pat p in
    let e = go e in
    (p, e)
  and go_pat (t, p) =
    let t = lower_type @@ clone_inst ctx.fresh_tvar ty_cache t in
    let p =
      match p with
      | C.PVar x -> PVar x
      | C.PTag (tag, args) ->
          let args = List.map go_pat args in
          PTag (tag, args)
    in
    (t, p)
  in
  go expr

let specialize_let_fn ~ctx ~ty_cache ~mono_cache ~t_new ~name_new
    (C.Letfn { recursive; bind = t, name; arg = t_arg, arg_sym; body; _ }) =
  Option.iter (fun r -> assert (r = name)) recursive;
  let t = clone_inst ctx.fresh_tvar ty_cache t in
  Canonical_solved.Lower.unify ctx.fresh_tvar t t_new;
  let t = lower_type mono_cache t in

  let t_arg =
    lower_type mono_cache @@ clone_inst ctx.fresh_tvar ty_cache t_arg
  in
  let body = specialize_expr ~ctx ~ty_cache ~mono_cache body in
  let recursive = Option.is_some recursive in
  let letfn =
    Letfn { recursive; bind = (t, name_new); arg = (t_arg, arg_sym); body }
  in
  `Letfn letfn

let fresh_ty_cache () : ty_cache = ref []

let specialize_let_val ctx (C.Letval { bind = t, name; body; _ }) =
  let ty_cache = fresh_ty_cache () in
  let mono_cache = fresh_mono_cache () in
  let t = lower_type mono_cache @@ clone_inst ctx.fresh_tvar ty_cache t in
  let body = specialize_expr ~ctx ~ty_cache ~mono_cache body in
  let letval = Letval { bind = (t, name); body } in
  `Def (`Letval letval)

let specialize_run_def ctx (C.Run { bind = t, name; body; _ }) =
  let ty_original = t in
  let ty_cache = fresh_ty_cache () in
  let mono_cache = fresh_mono_cache () in
  let t = lower_type mono_cache @@ clone_inst ctx.fresh_tvar ty_cache t in
  let body = specialize_expr ~ctx ~ty_cache ~mono_cache body in
  Run { bind = (t, name); body; ty = ty_original }

let make_context ~symbols ~fresh_tvar program =
  {
    symbols;
    fresh_tvar;
    specializations = Specializations.make symbols program;
  }

let loop_specializations : ctx -> unit =
 fun ctx ->
  let rec go () =
    match Specializations.next_needed_specialization ctx.specializations with
    | None -> ()
    | Some { def; name_new; t_new; specialized } ->
        let ty_cache = fresh_ty_cache () in
        let mono_cache = fresh_mono_cache () in
        let def =
          specialize_let_fn ~ctx ~ty_cache ~mono_cache ~t_new ~name_new def
        in
        specialized := Some (`Def def);
        go ()
  in
  go ()

let lower : ctx -> C.program -> program =
 fun ctx program ->
  let let_defs =
    List.filter_map
      (function `Def (`Letval def) -> Some def | _ -> None)
      program
  in
  let run_defs =
    List.filter_map (function `Run run -> Some run | _ -> None) program
  in
  let let_defs = List.map (specialize_let_val ctx) let_defs in
  let run_defs = List.map (specialize_run_def ctx) run_defs in
  loop_specializations ctx;
  let other_defs = Specializations.solved_specializations ctx.specializations in
  other_defs @ let_defs @ List.map (fun d -> `Run d) run_defs
