open Ast
open Type
module C = Canonical.Ast

exception Solve_err of string

let failsolve f s = raise (Solve_err (f ^ ": " ^ s))

type ctx = { symbols : Symbol.t; fresh_tvar : fresh_tvar }
type venv = (Symbol.symbol * tvar) list

let noloc = Language.noloc

let is_generalized : tvar -> bool =
 fun t ->
  let visited = ref [] in
  let rec go t =
    let var = tvar_v t in
    if List.mem var !visited then false
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd _ -> false
      | Link t -> go t
      | ForA _ -> true
      | Content (TPrim _) | Content TTagEmpty -> false
      | Content (TTag { tags; ext }) ->
          let check_tag : ty_tag -> bool =
           fun (_, args) -> List.exists (fun (_, t) -> go t) args
          in
          List.exists check_tag tags || go (snd ext)
      | Content (TRecord { fields; ext }) ->
          let check_field : ty_field -> bool = fun (_, (_, t)) -> go t in
          List.exists check_field fields || go (snd ext)
      | Content TRecordEmpty -> false
      | Content (TFn ((_, t1), (_, t2))) -> go t1 || go t2
      | Alias { alias = _, args; real } ->
          List.exists (fun (_, t) -> go t) args || go real)
  in
  go t

let inst : ctx -> tvar -> tvar =
 fun ctx gt ->
  if not (is_generalized gt) then gt
  else
    let tenv : (variable * tvar) list ref = ref [] in
    let rec go : tvar -> tvar =
     fun gt ->
      let var = tvar_v gt in
      match List.assoc_opt var !tenv with
      | Some t -> t
      | None ->
          let t = ctx.fresh_tvar (Unbd None) in
          tenv := (var, t) :: !tenv;
          let t' =
            match tvar_deref gt with
            | Unbd _ -> gt
            | Link t -> go t
            | ForA x -> ctx.fresh_tvar (Unbd x)
            | Content (TPrim _) | Content TTagEmpty -> gt
            | Content (TTag { tags; ext = _, ext }) ->
                let map_tag : ty_tag -> ty_tag =
                 fun (tag, args) ->
                  let args = List.map (fun (_, t) -> (noloc, go t)) args in
                  (tag, args)
                in
                let tags = List.map map_tag tags in
                let ext = (noloc, go ext) in
                ctx.fresh_tvar @@ Content (TTag { tags; ext })
            | Content (TRecord { fields; ext = _, ext }) ->
                let map_field : ty_field -> ty_field =
                 fun (field, (_, t)) ->
                  let t' = go t in
                  (field, (noloc, t'))
                in
                let fields = List.map map_field fields in
                let ext = (noloc, go ext) in
                ctx.fresh_tvar @@ Content (TRecord { fields; ext })
            | Content TRecordEmpty -> ctx.fresh_tvar @@ Content TRecordEmpty
            | Content (TFn ((_, t1), (_, t2))) ->
                let t1 = (noloc, go t1) in
                let t2 = (noloc, go t2) in
                ctx.fresh_tvar @@ Content (TFn (t1, t2))
            | Alias { alias = name, args; real } ->
                let args = List.map (fun (_, t) -> (noloc, go t)) args in
                let real = go real in
                ctx.fresh_tvar @@ Alias { alias = (name, args); real }
          in
          tvar_set t (Link t');
          t
    in
    go gt

let inst_ksig : ctx -> kernel_sig -> kernel_sig =
 fun ctx { args; ret } ->
  let ret = inst ctx ret in
  let args =
    match args with
    | `Variadic t -> `Variadic (inst ctx t)
    | `List ts -> `List (List.map (inst ctx) ts)
  in
  { args; ret }

let occurs : variable -> tvar -> bool =
 fun v t ->
  let visited = ref [] in
  let rec go t =
    let var = tvar_v t in
    if List.mem var !visited then false
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd _ -> var = v
      | ForA _ ->
          (* generalized variables should never occur in the same scope in another variable *)
          assert (var <> v);
          false
      | Link t -> go t
      | Content (TPrim _) | Content TTagEmpty -> false
      | Content (TTag { tags; ext }) ->
          let check_tag : ty_tag -> bool =
           fun (_, args) -> List.exists (fun (_, t) -> go t) args
          in
          List.exists check_tag tags || go (snd ext)
      | Content (TRecord { fields; ext }) ->
          let check_field : ty_field -> bool = fun (_, (_, t)) -> go t in
          List.exists check_field fields || go (snd ext)
      | Content TRecordEmpty -> false
      | Content (TFn ((_, t1), (_, t2))) -> go t1 || go t2
      | Alias { alias = _, args; real } ->
          List.exists (fun (_, t) -> go t) args || go real)
  in
  go t

let gen : venv -> tvar -> unit =
 fun venv t ->
  let visited = ref [] in
  let rec go t =
    let var = tvar_v t in
    if List.mem var !visited then ()
    else (
      visited := var :: !visited;
      match tvar_deref t with
      | Unbd s ->
          if List.exists (fun (_, t) -> occurs var t) venv then
            (* variable occurs in the current env, don't generalize *)
            ()
          else tvar_set t (ForA s)
      | Link t -> go t
      | ForA _ -> ()
      | Content (TPrim _) | Content TTagEmpty -> ()
      | Content (TTag { tags; ext }) ->
          let gen_tag : ty_tag -> unit =
           fun (_, args) -> List.iter (fun (_, t) -> go t) args
          in
          List.iter gen_tag tags;
          go (snd ext)
      | Content (TRecord { fields; ext }) ->
          let gen_field : ty_field -> unit = fun (_, (_, t)) -> go t in
          List.iter gen_field fields;
          go (snd ext)
      | Content TRecordEmpty -> ()
      | Content (TFn ((_, t1), (_, t2))) ->
          go t1;
          go t2
      | Alias { alias = _, args; real } ->
          List.iter (fun (_, t) -> go t) args;
          go real)
  in
  go t

type 'a separated = {
  shared : ('a * 'a) list;
  only1 : 'a list;
  only2 : 'a list;
}

let separate ~(chase : 'a list -> tvar -> 'a list * tvar) (tags1 : 'a list)
    (ext1 : tvar) (tags2 : 'a list) (ext2 : tvar) : 'a separated * tvar * tvar =
  let tags1, ext1 = chase tags1 ext1 in
  let tags2, ext2 = chase tags2 ext2 in
  let tags1, tags2 = (Util.sort_tagged tags1, Util.sort_tagged tags2) in
  let rec walk shared only1 only2 = function
    | [], [] -> { shared; only1 = List.rev only1; only2 = List.rev only2 }
    | o :: rest, [] -> walk shared (o :: only1) only2 (rest, [])
    | [], o :: rest -> walk shared only1 (o :: only2) ([], rest)
    | t1 :: rest1, t2 :: rest2 when fst t1 < fst t2 ->
        walk shared (t1 :: only1) only2 (rest1, t2 :: rest2)
    | t1 :: rest1, t2 :: rest2 when fst t1 > fst t2 ->
        walk shared only1 (t2 :: only2) (t1 :: rest1, rest2)
    | t1 :: rest1, t2 :: rest2 ->
        walk ((t1, t2) :: shared) only1 only2 (rest1, rest2)
  in
  let result = walk [] [] [] (tags1, tags2) in
  (result, ext1, ext2)

let unify : fresh_tvar -> tvar -> tvar -> unit =
 fun fresh_tvar t u ->
  let rec unify_tags fresh_tvar visited (t1, args1) (t2, args2) =
    assert (t1 = t2);
    if List.length args1 <> List.length args2 then
      failsolve "arity mismatch for tag" t1;
    List.iter2 (unify fresh_tvar visited) (List.map snd args1)
      (List.map snd args2)
  and unify_fields fresh_tvar visited (f1, (_, t1)) (f2, (_, t2)) =
    assert (f1 = f2);
    unify fresh_tvar visited t1 t2
  and unify fresh_tvar visited t u =
    let t, u = (unlink t, unlink u) in
    let vart, varu = (tvar_v t, tvar_v u) in
    if vart = varu then ()
    else if List.mem (vart, varu) visited then
      (*
      failsolve "recursive type variable"
        ("found recursive type variable " ^ show_tvar t)
      *)
      ()
    else
      let visited = (vart, varu) :: visited in
      let unify = unify fresh_tvar visited in
      let t' =
        match (tvar_deref t, tvar_deref u) with
        | Link _, _ | _, Link _ ->
            failsolve "found a link where none was expected"
              (show_tvar t ^ " ~ " ^ show_tvar u)
        | ForA _, _ | _, ForA _ ->
            failsolve "cannot unify generalized type; forgot to instantiate?"
              (show_tvar t ^ " ~ " ^ show_tvar u)
        | Unbd None, Unbd (Some x) | Unbd (Some x), Unbd None -> Unbd (Some x)
        | Unbd _, other | other, Unbd _ -> other
        | _, Alias { alias; real } ->
            unify t real;
            Alias { alias; real }
        | Alias { alias; real }, _ ->
            unify real u;
            Alias { alias; real }
        | Content c1, Content c2 ->
            let c' =
              match (c1, c2) with
              | TPrim p1, TPrim p2 when p1 = p2 -> TPrim p1
              | TTagEmpty, TTagEmpty -> TTagEmpty
              | TTagEmpty, TTag { tags = []; ext = _, ext } ->
                  unify t ext;
                  TTagEmpty
              | TTag { tags = []; ext = _, ext }, TTagEmpty ->
                  unify u ext;
                  TTagEmpty
              | ( TTag { tags = tags1; ext = _, ext1 },
                  TTag { tags = tags2; ext = _, ext2 } ) -> (
                  let ({ shared; only1; only2 } : ty_tag separated), ext1, ext2
                      =
                    separate ~chase:chase_tags tags1 ext1 tags2 ext2
                  in
                  let shared : ty_tag list =
                    List.map
                      (fun (t1, t2) ->
                        unify_tags fresh_tvar visited t1 t2;
                        t1)
                      shared
                  in
                  match ((only1, ext1), (only2, ext2)) with
                  | ([], ext1), ([], ext2) ->
                      unify ext1 ext2;
                      let tags = Util.sort_tagged shared in
                      TTag { tags; ext = (noloc, ext1) }
                  | (others, ext1), ([], ext2) | ([], ext2), (others, ext1) ->
                      let other_tag_union =
                        fresh_tvar
                        @@ Content (TTag { tags = others; ext = (noloc, ext1) })
                      in
                      unify ext2 other_tag_union;
                      let tags = Util.sort_tagged @@ shared @ others in
                      TTag { tags; ext = (noloc, ext1) }
                  | (others1, ext1), (others2, ext2) ->
                      let new_ext = (noloc, fresh_tvar @@ Unbd None) in
                      let tags1 =
                        fresh_tvar
                        @@ Content (TTag { tags = others1; ext = new_ext })
                      in
                      let tags2 =
                        fresh_tvar
                        @@ Content (TTag { tags = others2; ext = new_ext })
                      in
                      unify ext1 tags2;
                      unify ext2 tags1;

                      let all_tags =
                        Util.sort_tagged @@ shared @ others1 @ others2
                      in
                      TTag { tags = all_tags; ext = new_ext })
              | TRecordEmpty, TRecordEmpty -> TRecordEmpty
              | TRecordEmpty, TRecord { fields = []; ext = _, ext } ->
                  unify t ext;
                  TRecordEmpty
              | TRecord { fields = []; ext = _, ext }, TRecordEmpty ->
                  unify u ext;
                  TRecordEmpty
              | ( TRecord { fields = fields1; ext = _, ext1 },
                  TRecord { fields = fields2; ext = _, ext2 } ) -> (
                  let ( ({ shared; only1; only2 } : ty_field separated),
                        ext1,
                        ext2 ) =
                    separate ~chase:chase_fields fields1 ext1 fields2 ext2
                  in
                  let shared : ty_field list =
                    List.iter
                      (fun (f1, f2) -> unify_fields fresh_tvar visited f1 f2)
                      shared;
                    List.map fst shared
                  in
                  match ((only1, ext1), (only2, ext2)) with
                  | ([], ext1), ([], ext2) ->
                      unify ext1 ext2;
                      let fields = Util.sort_tagged shared in
                      TRecord { fields; ext = (noloc, ext1) }
                  | (others, ext1), ([], ext2) | ([], ext2), (others, ext1) ->
                      let other_record =
                        fresh_tvar
                        @@ Content
                             (TRecord { fields = others; ext = (noloc, ext1) })
                      in
                      unify ext2 other_record;
                      let fields = Util.sort_tagged @@ shared @ others in
                      TRecord { fields; ext = (noloc, ext1) }
                  | (others1, ext1), (others2, ext2) ->
                      let new_ext = (noloc, fresh_tvar @@ Unbd None) in
                      let fields1 =
                        fresh_tvar
                        @@ Content (TRecord { fields = others1; ext = new_ext })
                      in
                      let fields2 =
                        fresh_tvar
                        @@ Content (TRecord { fields = others2; ext = new_ext })
                      in
                      unify ext1 fields2;
                      unify ext2 fields1;

                      let all_fields =
                        Util.sort_tagged @@ shared @ others1 @ others2
                      in
                      TRecord { fields = all_fields; ext = new_ext })
              | TFn ((_, ta1), (_, tr1)), TFn ((_, ta2), (_, tr2)) ->
                  unify ta1 ta2;
                  unify tr1 tr2;
                  TFn ((noloc, ta1), (noloc, tr1))
              | _ -> failsolve "incompatible" (show_tvar t ^ " ~ " ^ show_tvar u)
            in
            Content c'
      in
      let v = fresh_tvar @@ Unbd None in
      tvar_set t (Link v);
      tvar_set u (Link v);
      tvar_set v t'
  in
  unify fresh_tvar [] t u

let constrain_sig : ctx -> sig_:tvar option -> t:tvar -> unit =
 fun ctx ~sig_ ~t ->
  match sig_ with
  | Some sig_ ->
      let t_sig = inst ctx sig_ in
      unify ctx.fresh_tvar t t_sig
  | None -> ()

let rec infer_expr : ctx -> venv -> e_expr -> tvar =
 fun ctx venv (t, e) ->
  let t' =
    match e with
    | Str _ -> ctx.fresh_tvar @@ Content (TPrim `Str)
    | Int _ -> ctx.fresh_tvar @@ Content (TPrim `Int)
    | Var x -> (
        match List.assoc_opt x venv with
        | Some t -> inst ctx t
        | None ->
            failsolve "infer_expr"
              ("unbound variable " ^ Symbol.syn_of ctx.symbols x))
    | Tag (tag, args) ->
        let arg_tys =
          List.map (fun t -> (noloc, t)) @@ List.map (infer_expr ctx venv) args
        in
        let ext = (noloc, ctx.fresh_tvar @@ Unbd None) in
        ctx.fresh_tvar @@ Content (TTag { tags = [ (tag, arg_tys) ]; ext })
    | Record fields ->
        let go_field (f, e) =
          let t = infer_expr ctx venv e in
          (f, (noloc, t))
        in
        let field_tys = List.map go_field fields in
        let ext = (noloc, ctx.fresh_tvar @@ Unbd None) in
        ctx.fresh_tvar @@ Content (TRecord { fields = field_tys; ext })
    | Access (e, f) ->
        let t_e = infer_expr ctx venv e in
        let t = ctx.fresh_tvar @@ Unbd None in
        let record_wanted =
          TRecord
            {
              fields = [ (f, (noloc, t)) ];
              ext = (noloc, ctx.fresh_tvar @@ Unbd None);
            }
        in
        let t_e_wanted = ctx.fresh_tvar @@ Content record_wanted in
        unify ctx.fresh_tvar t_e t_e_wanted;
        t
    | Let (let_def, rest) ->
        let let_def_t = infer_let_def ~nested:true ctx venv let_def in
        let let_def_s = name_of_let_def let_def in
        let venv' = (let_def_s, let_def_t) :: venv in
        infer_expr ctx venv' rest
    | Clos { arg = t_a, a; body } ->
        let t_ret = infer_expr ctx ((a, t_a) :: venv) body in
        let t_fn = ctx.fresh_tvar @@ Unbd None in
        tvar_set t_fn @@ Content (TFn ((noloc, t_a), (noloc, t_ret)));
        t_fn
    | Call (f, a) ->
        let t_f = infer_expr ctx venv f in
        let t_a = infer_expr ctx venv a in
        let t_ret = ctx.fresh_tvar @@ Unbd None in
        let t_f_wanted =
          ctx.fresh_tvar @@ Content (TFn ((noloc, t_a), (noloc, t_ret)))
        in
        unify ctx.fresh_tvar t_f t_f_wanted;
        t_ret
    | KCall (kernelfn, args) ->
        let ({ args = kargs; ret = kret } : kernel_sig) =
          inst_ksig ctx @@ kernel_sig kernelfn
        in
        let arg_tys = List.map (infer_expr ctx venv) @@ args in
        (match kargs with
        | `Variadic t -> List.iter (unify ctx.fresh_tvar t) arg_tys
        | `List kargs -> List.iter2 (unify ctx.fresh_tvar) kargs arg_tys);
        kret
    | When (e, bs) ->
        let t_e = infer_expr ctx venv e in
        let t_result = ctx.fresh_tvar @@ Unbd None in
        let go_branch (p, body) =
          let venv', t_p = infer_pat ctx venv p in
          unify ctx.fresh_tvar t_e t_p;
          let t_body = infer_expr ctx (venv' @ venv) body in
          unify ctx.fresh_tvar t_result t_body
        in
        List.iter go_branch bs;
        t_result
  in
  unify ctx.fresh_tvar t t';
  t

and infer_pat : ctx -> venv -> e_pat -> venv * tvar =
 fun ctx venv (t, p) ->
  let venv, t' =
    match p with
    | PTag (tag, args) ->
        let arg_venvs, arg_tys =
          List.split @@ List.map (infer_pat ctx venv) args
        in
        let args_venv = List.concat arg_venvs in
        let tag = (tag, List.map (fun t -> (noloc, t)) arg_tys) in
        let ext = (noloc, ctx.fresh_tvar @@ Unbd None) in
        let tag_ty = TTag { tags = [ tag ]; ext } in
        let t = ctx.fresh_tvar @@ Content tag_ty in
        (args_venv, t)
    | PVar x ->
        let t = ctx.fresh_tvar @@ Unbd None in
        ([ (x, t) ], t)
  in
  unify ctx.fresh_tvar t t';
  (venv, t)

and infer_let_def : nested:bool -> ctx -> venv -> let_def -> tvar =
 fun ~nested ctx venv -> function
  | `Letfn (Letfn { recursive; bind = t_x, x; arg = t_a, a; body; sig_ }) ->
      let recursive_binding =
        match recursive with
        | Some y ->
            assert (x = y);
            [ (x, t_x) ]
        | None -> []
      in
      let t_ret =
        let venv = (a, t_a) :: (recursive_binding @ venv) in
        infer_expr ctx venv body
      in
      let t_fn = ctx.fresh_tvar @@ Unbd None in

      tvar_set t_fn @@ Content (TFn ((noloc, t_a), (noloc, t_ret)));

      constrain_sig ctx ~sig_ ~t:t_fn;

      unify ctx.fresh_tvar t_fn t_x;
      if not nested then gen venv t_x;
      t_x
  | `Letval (Letval { bind = t_x, _; body; sig_ }) ->
      let t_body = infer_expr ctx venv body in

      constrain_sig ctx ~sig_ ~t:t_body;

      unify ctx.fresh_tvar t_body t_x;
      t_x

let infer_run_def : ctx -> venv -> run_def -> tvar =
 fun ctx venv (Run { bind = t_x, _; body; sig_ }) ->
  let t_body = infer_expr ctx venv body in

  constrain_sig ctx ~sig_ ~t:t_body;

  unify ctx.fresh_tvar t_body t_x;
  t_x

let infer_def : ctx -> venv -> def -> tvar =
 fun ctx venv -> function
  | `Def let_def -> infer_let_def ~nested:false ctx venv let_def
  | `Run run_def -> infer_run_def ctx venv run_def

let infer : ctx -> program -> unit =
 fun ctx program ->
  let rec go venv = function
    | [] -> ()
    | def :: defs ->
        let t = infer_def ctx venv def in
        let def_s = name_of_def def in
        go ((def_s, t) :: venv) defs
  in
  go [] program

let lower : ctx -> C.program -> program =
 fun ctx program ->
  infer ctx program;
  program
