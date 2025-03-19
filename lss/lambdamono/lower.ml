open Ast
open Lower_type
open Type_clone_inst
open Ctx
open Symbol
module M = Lambdasolved.Ast
module T = Lambdasolved.Type
module P = Lambdasolved.Type_print
module PM = Lambdasolved.Print

type venv = (symbol * T.tvar) list

let show_venv venv =
  List.map (fun (x, t) -> Symbol.show_symbol_raw x ^ ": " ^ P.show_ty t) venv
  |> String.concat ", "

let lookup_env venv x =
  match List.assoc_opt x venv with
  | Some t -> t
  | None ->
      failwith @@ "lookup_env: variable " ^ Symbol.show_symbol_raw x
      ^ " not found in venv " ^ show_venv venv

let specialize_expr ~(ctx : Ctx.t) ~ty_cache ~mono_cache ~(venv : venv) expr =
  let lower_type = lower_type mono_cache ctx.fresh_tvar in
  let rec go (venv : venv) (t, e) =
    let t = clone_inst ctx.s_fresh_tvar ty_cache t in
    let e =
      match e with
      | M.Var x -> (
          match Specializations.lookup_fn ctx.specializations x with
          | None -> Var x (* No specialization needed *)
          | Some fn -> (
              match lambda_repr t with
              | `LSet _ -> (
                  let _fn_sym =
                    Specializations.specialize_fn_lset ctx.specializations
                      mono_cache ctx.fresh_tvar x t
                  in
                  (* construct the lambda set tag *)
                  match extract_lset_fn mono_cache ctx.fresh_tvar t x with
                  | `Toplevel ->
                      let tag_name = lambda_tag_name x in
                      Tag (tag_name, [])
                  | `LSet { captures; ty = t_captures } ->
                      let tag_name = lambda_tag_name x in
                      let build_field (x, t) =
                        (Symbol.show_symbol_raw x, (lower_type t, Var x))
                      in
                      let captures_rcd =
                        Record (List.map build_field captures)
                      in
                      Tag (tag_name, [ (t_captures, captures_rcd) ]))
              | `LErased ->
                  let captures =
                    List.map fst @@ SymbolMap.bindings fn.captures
                  in
                  let captures =
                    List.map (fun x -> (x, lookup_env venv x)) captures
                  in
                  let e_captures =
                    List.map
                      (fun (x, t) ->
                        (Symbol.show_symbol_raw x, (lower_type t, Var x)))
                      captures
                  in
                  let captures_rcd =
                    if List.length e_captures = 0 then None
                    else
                      let captures_rcd = Record e_captures in
                      let t_captures =
                        List.map (fun (f, t) -> (f, fst t)) e_captures
                      in
                      let t_captures = ctx.fresh_tvar (TRecord t_captures) in
                      Some (t_captures, captures_rcd)
                  in
                  let fn_sym =
                    Specializations.specialize_fn_erased ctx.specializations
                      mono_cache ctx.fresh_tvar x t captures
                  in
                  PackedFn { lambda = fn_sym; captures = captures_rcd }))
      | M.Int i -> Int i
      | M.Str s -> Str s
      | M.Tag (t, args) ->
          let args = List.map (go venv) args in
          Tag (t, args)
      | M.Record fields ->
          let fields = List.map (fun (f, e) -> (f, go venv e)) fields in
          Record fields
      | M.Access (e, f) ->
          let e = go venv e in
          Access (e, f)
      | M.Let ((t_x, x), body, rest) ->
          let t_x = clone_inst ctx.s_fresh_tvar ty_cache t_x in
          let body = go venv body in
          let rest = go ((x, t_x) :: venv) rest in
          Let ((lower_type t_x, x), body, rest)
      | M.Call (((t_f, _) as f), a) -> (
          let t_f = clone_inst ctx.s_fresh_tvar ty_cache t_f in
          let f = go venv f in
          let a = go venv a in
          let compile_branch ((lambda, _) : symbol * T.captures) : branch =
            let captures_sym = ctx.symbols.fresh_symbol_named "captures" in
            let t_captures =
              extract_lset_fn mono_cache ctx.fresh_tvar t_f lambda
            in
            let lambda_real =
              Specializations.specialize_fn_lset ctx.specializations mono_cache
                ctx.fresh_tvar lambda t_f
            in
            match t_captures with
            | `Toplevel ->
                let pat = (fst f, PTag (lambda_tag_name lambda, [])) in
                let body = (fst a, Call (lambda_real, [ a ])) in
                (pat, body)
            | `LSet { ty = t_captures; _ } ->
                let pat =
                  ( fst f,
                    PTag
                      ( lambda_tag_name lambda,
                        [ (t_captures, PVar captures_sym) ] ) )
                in
                let body =
                  ( fst a,
                    Call (lambda_real, [ a; (t_captures, Var captures_sym) ]) )
                in
                (pat, body)
          in
          match lambda_repr t_f with
          | `LSet lambda_set ->
              let branches =
                SymbolMap.bindings lambda_set |> List.map compile_branch
              in
              When (f, branches)
          | `LErased -> CallIndirect (f, [ a ]))
      | M.KCall (kfn, args) ->
          let args = List.map (go venv) args in
          KCall (kfn, args)
      | M.When (e, branches) ->
          let e = go venv e in
          let branches = List.map (go_branch venv) branches in
          When (e, branches)
    in
    (lower_type t, e)
  and go_branch venv (p, e) =
    let p, venv' = go_pat p in
    let e = go (venv' @ venv) e in
    (p, e)
  and go_pat (t, p) =
    let t = clone_inst ctx.s_fresh_tvar ty_cache t in
    let p, venv =
      match p with
      | M.PVar x -> (PVar x, [ (x, t) ])
      | M.PTag (tag, args) ->
          let args, venvs = List.split @@ List.map go_pat args in
          (PTag (tag, args), List.concat venvs)
    in
    ((lower_type t, p), venv)
  in
  go venv expr

let fresh_ty_cache () = ref []

let specialize_fn ~ctx ~ty_cache ~mono_cache ~t_new ~lambda ~t ~captures_new
    ({ arg = t_arg, arg; captures; body } : M.fn) =
  let lower_type = lower_type mono_cache ctx.fresh_tvar in
  let clone_inst = clone_inst ctx.s_fresh_tvar ty_cache in

  let t = clone_inst t in
  Lambdasolved.Solve.unify ctx.s_fresh_tvar t t_new;

  let t_arg = clone_inst t_arg in

  match lambda_repr t with
  | `LSet _ -> (
      let captures = extract_lset_fn mono_cache ctx.fresh_tvar t lambda in
      match captures with
      | `Toplevel ->
          let venv = [ (arg, t_arg) ] in
          let body = specialize_expr ~ctx ~ty_cache ~mono_cache ~venv body in
          let t_arg = lower_type t_arg in
          { args = [ (t_arg, arg) ]; body }
      | `LSet { captures; ty = t_captures } ->
          let venv = (arg, t_arg) :: captures in
          let body = specialize_expr ~ctx ~ty_cache ~mono_cache ~venv body in
          let captures_sym = ctx.symbols.fresh_symbol_named "captures" in
          let args = [ (lower_type t_arg, arg); (t_captures, captures_sym) ] in
          let body =
            List.fold_left
              (fun body (x, t) ->
                let t = lower_type t in
                let captures_arg = (t_captures, Var captures_sym) in
                let access =
                  (t, Access (captures_arg, Symbol.show_symbol_raw x))
                in
                let bind = (t, x) in
                (fst body, Let (bind, access, body)))
              body captures
          in
          { args; body })
  | `LErased ->
      let captures = Util.sort_tagged @@ SymbolMap.bindings captures in
      let captures = List.map (fun (x, t) -> (x, clone_inst t)) captures in
      let captures_new = Option.value ~default:[] captures_new in
      let captures_new = Util.sort_tagged @@ captures_new in
      List.iter2
        (fun (x, t) (x', t') ->
          assert (x = x');
          Lambdasolved.Solve.unify ctx.s_fresh_tvar t t')
        captures captures_new;
      let venv = (arg, t_arg) :: captures in
      if List.length captures = 0 then
        let t_arg = lower_type t_arg in
        let body = specialize_expr ~ctx ~ty_cache ~mono_cache ~venv body in
        { args = [ (t_arg, arg) ]; body }
      else
        let body = specialize_expr ~ctx ~ty_cache ~mono_cache ~venv body in
        let captures_sym = ctx.symbols.fresh_symbol_named "captures" in
        let t_captures = lower_captures mono_cache ctx.fresh_tvar captures in
        let args = [ (lower_type t_arg, arg); (t_captures, captures_sym) ] in
        let body =
          List.fold_left
            (fun body (x, t) ->
              let t = lower_type t in
              let captures_arg = (t_captures, Var captures_sym) in
              let access =
                (t, Access (captures_arg, Symbol.show_symbol_raw x))
              in
              let bind = (t, x) in
              (fst body, Let (bind, access, body)))
            body captures
        in
        { args; body }

let specialize_val ~ctx ~ty_cache ~mono_cache body =
  let body = specialize_expr ~ctx ~ty_cache ~mono_cache body in
  body

let specialize_run ~ctx ~ty_cache ~mono_cache body =
  let body = specialize_val ~ctx ~ty_cache ~mono_cache body in
  body

let loop_specializations : Ctx.t -> unit =
 fun ctx ->
  let rec go () =
    match Specializations.next_specialization ctx.specializations with
    | None -> ()
    | Some { name; t_fn; fn; t_new; specialized; name_new = _; captures_new } ->
        let fn =
          specialize_fn ~ctx ~ty_cache:(fresh_ty_cache ())
            ~mono_cache:(fresh_mono_cache ()) ~t_new ~lambda:name ~t:t_fn
            ~captures_new fn
        in
        specialized := Some fn;
        go ()
  in
  go ()

let init_specializations : Ctx.t -> M.program -> def list =
 fun ctx program ->
  let rec go (acc : def list) = function
    | [] -> List.rev acc
    | ((_, x), def) :: defs ->
        let acc =
          match def with
          | `Run (run, t) ->
              let ty_cache = fresh_ty_cache () in
              let mono_cache = fresh_mono_cache () in
              let venv = [] in
              let run = specialize_run ~ctx ~ty_cache ~mono_cache ~venv run in
              let acc = (x, `Run (run, t)) :: acc in
              acc
          | `Val val_ ->
              let ty_cache = fresh_ty_cache () in
              let mono_cache = fresh_mono_cache () in
              let venv = [] in
              let val_ = specialize_val ~ctx ~ty_cache ~mono_cache ~venv val_ in
              let acc = (x, `Val val_) :: acc in
              acc
          | `Fn _ ->
              (* these will get specialized when called *)
              acc
        in
        go acc defs
  in
  go [] program

let lower : Ctx.t -> M.program -> program =
 fun ctx program ->
  let val_run_defs = init_specializations ctx program in
  loop_specializations ctx;
  let fn_defs = Specializations.solved_specializations ctx.specializations in
  fn_defs @ val_run_defs
