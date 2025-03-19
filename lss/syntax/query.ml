open Symbol
open Type
open Ast
open Language

type node_kind =
  [ `Def of symbol | `Var of symbol | `Alias of symbol | `Generic ]

type found_node = (loc * tvar * node_kind) option

let or_else o f = match o with Some a -> Some a | None -> f ()

let tightest_node_at_var : loc -> loc_tvar -> found_node =
 fun loc loc_ty ->
  let rec go_tag (_tag, args) : found_node = List.find_map go args
  and go (l, ty) : found_node =
    let deeper =
      match tvar_deref ty with
      | Link ty -> go (l, ty)
      | Unbd _ | ForA _ -> None
      | Content (TPrim _) -> None
      | Content TTagEmpty -> None
      | Content (TTag { tags; ext }) ->
          let found_in_tag = List.find_map go_tag tags in
          or_else found_in_tag (fun () -> go ext)
      | Content (TRecord { fields; ext }) ->
          let found_in_record = List.find_map go @@ List.map snd fields in
          or_else found_in_record (fun () -> go ext)
      | Content TRecordEmpty -> None
      | Content (TFn (in', out)) -> or_else (go in') (fun () -> go out)
      | Alias { alias = (l_x, x), vars; real = _ } ->
          if within loc l_x then Some (l_x, ty, `Alias x)
          else List.find_map go vars
    in
    let surface () = if within loc l then Some (l, ty, `Generic) else None in
    or_else deeper surface
  in
  go loc_ty

let tightest_node_at_expr : loc -> e_expr -> found_node =
 fun loc program ->
  let rec pat (l, ty, p) : found_node =
    let deeper =
      match p with
      | PTag (_, args) -> List.find_map pat args
      | PVar (l, x) -> if within loc l then Some (l, ty, `Var x) else None
    in
    or_else deeper (fun () ->
        if within loc l then Some (l, ty, `Generic) else None)
  in
  let rec expr (l, ty, e) : found_node =
    let deeper =
      match e with
      | Var _ | Int _ | Str _ -> None
      | Let { recursive = _; bind = l, ty, x; expr = e1; body = e2 } ->
          if within loc l then Some (l, snd ty, `Def x)
          else or_else (expr e1) (fun () -> expr e2)
      | Tag (_, tags) -> List.find_map (fun tag -> expr tag) tags
      | Record fields -> List.find_map (fun (_, e) -> expr e) fields
      | Access (e, _) -> expr e
      | Clos { arg = l, ty, x; body = e } ->
          if within loc l then Some (l, snd ty, `Var x) else expr e
      | Call (e1, e2) -> or_else (expr e1) (fun () -> expr e2)
      | KCall (_, es) -> List.find_map (fun e -> expr e) es
      | When (e, branches) ->
          let check_branch (pat', body) =
            or_else (pat pat') (fun () -> expr body)
          in
          or_else (expr e) (fun () -> List.find_map check_branch branches)
    in
    or_else deeper (fun () ->
        if within loc l then
          let kind = match e with Var x -> `Var x | _ -> `Generic in
          Some (l, ty, kind)
        else None)
  in
  expr program

let tightest_node_at_def : loc -> e_def -> found_node =
 fun loc (l, ty, def) ->
  let deeper =
    match def with
    | TyAlias ((l_x, x), vars, var) ->
        if within loc l_x then Some (l_x, ty, `Alias x)
        else
          or_else
            (List.find_map (tightest_node_at_var loc) vars)
            (fun () -> tightest_node_at_var loc var)
    | Sig ((l_x, x), var) ->
        if within loc l_x then Some (l_x, snd var, `Def x)
        else tightest_node_at_var loc var
    | Def ((l_x, x), e) | Run ((l_x, x), e) ->
        if within loc l_x then Some (l_x, ty, `Def x)
        else tightest_node_at_expr loc e
  in
  let surface () =
    if within loc l then
      let kind =
        match def with
        | TyAlias ((_, x), _, _) -> `Alias x
        | Sig ((_, x), _) | Def ((_, x), _) | Run ((_, x), _) -> `Def x
      in
      Some (l, ty, kind)
    else None
  in
  or_else deeper surface

let tightest_node_at_program : loc -> program -> found_node =
 fun loc program -> List.find_map (tightest_node_at_def loc) program

let type_at : loc -> program -> tvar option =
 fun loc program ->
  let found = tightest_node_at_program loc program in
  match found with Some (l, ty, _) when l = loc -> Some ty | _ -> None

let hover_info lineco program symbols =
  let open Printf in
  let wrap_code code = sprintf "```compose_fx\n%s\n```" code in
  let gen_docs (range, ty, kind) =
    let names = Type_print.name_vars [ ty ] in
    let ty_str = Type_print.string_of_tvar default_width symbols names ty in
    let split =
      if List.length @@ String.split_on_char '\n' ty_str > 0 then "\n" else " "
    in
    let prefix =
      match kind with
      | `Var x -> sprintf "(var) %s:%s" (Symbol.syn_of symbols x) split
      | `Def x -> sprintf "(def) %s:%s" (Symbol.syn_of symbols x) split
      | `Alias x -> sprintf "(alias) %s:%s" (Symbol.syn_of symbols x) split
      | `Generic -> ""
    in
    let ty_doc = prefix ^ ty_str in
    let md_docs = [ wrap_code ty_doc ] in
    { range; md_docs }
  in
  let node = tightest_node_at_program (lineco, lineco) program in
  Option.map gen_docs node
