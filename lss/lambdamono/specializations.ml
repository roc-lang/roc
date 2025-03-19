open Symbol
open Type
open Ast
open Lower_type
module M = Lambdasolved.Ast
module T = Lambdasolved.Type

type specialization_key = {
  name : symbol;
  arg : tvar;
  ret : tvar;
  captures_spec : [ `Toplevel | `LSet of tvar | `LErased of tvar ];
}
[@@deriving eq]

type specialization = {
  name : symbol;
  t_fn : T.tvar;
  fn : M.fn;
  t_new : T.tvar;
  name_new : symbol;
  (* Some if we're building an erased specialization *)
  captures_new : (symbol * T.tvar) list option;
  specialized : fn option ref;
}

type t = {
  symbols : Symbol.t;
  fenv : (symbol * (T.tvar * M.fn)) list;
  specializations : (specialization_key * specialization) list ref;
}

let make : Symbol.t -> M.program -> t =
 fun symbols program ->
  let fenv =
    List.filter_map
      (function (t, name), `Fn fn -> Some (name, (t, fn)) | _ -> None)
      program
  in
  { symbols; fenv; specializations = ref [] }

let lookup_fn : t -> symbol -> M.fn option =
 fun t name -> List.assoc_opt name t.fenv |> Option.map snd

let specialize_fn_lset :
    t -> mono_cache -> fresh_tvar -> symbol -> T.tvar -> symbol =
 fun t mono_cache fresh_tvar name t_new ->
  let t_fn, fn = List.assoc name t.fenv in
  let captures_spec =
    match extract_lset_fn mono_cache fresh_tvar t_new name with
    | `Toplevel -> `Toplevel
    | `LSet captures -> `LSet captures.ty
  in
  let in', _lset, out' = extract_fn t_new in
  let specialization_key =
    {
      name;
      arg = lower_type mono_cache fresh_tvar in';
      ret = lower_type mono_cache fresh_tvar out';
      captures_spec;
    }
  in
  let matched =
    List.find_opt (fun (key, _) ->
        equal_specialization_key key specialization_key)
    @@ !(t.specializations)
  in
  match matched with
  | Some (_, { name_new; _ }) -> name_new
  | None ->
      let name_new =
        t.symbols.fresh_symbol_named @@ Symbol.syn_of t.symbols name
      in
      let specialization =
        {
          name;
          t_fn;
          fn;
          name_new;
          t_new;
          specialized = ref None;
          captures_new = None;
        }
      in
      t.specializations :=
        (specialization_key, specialization) :: !(t.specializations);
      name_new

let specialize_fn_erased :
    t ->
    mono_cache ->
    fresh_tvar ->
    symbol ->
    T.tvar ->
    (symbol * T.tvar) list ->
    symbol =
 fun t mono_cache fresh_tvar name t_new captures ->
  let lower_type = lower_type mono_cache fresh_tvar in

  let t_fn, fn = List.assoc name t.fenv in
  let in', _lset, out' = extract_fn t_new in
  let t_captures =
    List.map (fun (f, t) -> (Symbol.show_symbol_raw f, lower_type t)) captures
  in
  let t_captures = fresh_tvar (TRecord t_captures) in
  let captures_spec =
    if List.length captures = 0 then `Toplevel else `LErased t_captures
  in
  let specialization_key =
    { name; arg = lower_type in'; ret = lower_type out'; captures_spec }
  in
  let matched =
    List.find_opt (fun (key, _) ->
        equal_specialization_key key specialization_key)
    @@ !(t.specializations)
  in
  match matched with
  | Some (_, { name_new; _ }) -> name_new
  | None ->
      let name_new =
        t.symbols.fresh_symbol_named @@ Symbol.syn_of t.symbols name
      in
      let specialization =
        {
          name;
          t_fn;
          fn;
          name_new;
          t_new;
          specialized = ref None;
          captures_new = Some captures;
        }
      in
      t.specializations :=
        (specialization_key, specialization) :: !(t.specializations);
      name_new

let solved_specializations : t -> def list =
 fun t ->
  !(t.specializations) |> List.map snd
  |> List.filter_map (fun { specialized; name_new; _ } ->
         Option.map (fun fn -> (name_new, `Fn fn)) !specialized)

let next_specialization : t -> specialization option =
 fun t ->
  List.find_opt (fun { specialized; _ } -> !specialized = None)
  @@ List.map snd !(t.specializations)
