open Symbol
open Type
open Ast
open Lower_type
module C = Canonical_solved.Ast
module T = Canonical_solved.Type

type needed_specialization = {
  def : C.letfn;
  name_new : symbol;
  t_new : T.tvar;
  specialized : def option ref;
}

type specialization_key = symbol * ty [@@deriving eq]

type t = {
  symbols : Symbol.t;
  fenv : (symbol * C.letfn) list;
  specializations : (specialization_key * needed_specialization) list ref;
}

let make : Symbol.t -> C.program -> t =
 fun symbols program ->
  let fenv =
    List.filter_map
      (function
        | `Def (`Letfn leftfn) -> Some (C.name_of_letfn leftfn, leftfn)
        | `Def (`Letval _) -> None
        | `Run _ -> None)
      program
  in
  { symbols; fenv; specializations = ref [] }

let specialize_fn : t -> mono_cache -> symbol -> T.tvar -> symbol option =
 fun t mono_cache name t_new ->
  let specialization = (name, lower_type mono_cache t_new) in

  match
    List.find_opt
      (fun (key, _) -> equal_specialization_key key specialization)
      !(t.specializations)
  with
  | Some (_, { name_new; _ }) -> Some name_new
  | None ->
      let ( let* ) = Option.bind in
      let* def = List.assoc_opt name t.fenv in
      let name_new =
        t.symbols.fresh_symbol_named @@ Symbol.syn_of t.symbols name
      in
      let needed_specialization =
        { def; name_new; t_new; specialized = ref None }
      in
      t.specializations :=
        (specialization, needed_specialization) :: !(t.specializations);
      Some name_new

let solved_specializations : t -> def list =
 fun t ->
  !(t.specializations) |> List.map snd
  |> List.filter_map (fun { specialized; _ } -> !specialized)

let next_needed_specialization : t -> needed_specialization option =
 fun t ->
  List.find_opt (fun { specialized; _ } -> !specialized = None)
  @@ List.map snd !(t.specializations)
