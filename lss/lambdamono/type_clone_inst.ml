open Symbol
open Lambdasolved.Type

type ty_cache = (variable * tvar) list ref

let show_ty_cache (cache : ty_cache) =
  let show_entry (var, tvar) =
    "(" ^ show_variable var ^ ", "
    ^ Lambdasolved.Type_print.show_tvar tvar
    ^ ")"
  in
  List.map show_entry !cache |> String.concat ", "

let clone_inst : fresh_tvar -> ty_cache -> tvar -> tvar =
 fun fresh_tvar cache ty ->
  let rec go : tvar -> tvar =
   fun tvar ->
    let { var; ty } = unlink tvar in
    match List.assoc_opt var !cache with
    | Some tvar -> tvar
    | None ->
        let tvar = fresh_tvar @@ Unbd in
        cache := (var, tvar) :: !cache;

        let ty =
          match !ty with
          | Link _ -> failwith "clone_type: Link"
          | Unbd -> Unbd
          | ForA -> Unbd
          | Content (LSet lambdas) ->
              let go_captures captures = SymbolMap.map go captures in
              let lambdas = SymbolMap.map go_captures lambdas in
              Content (LSet lambdas)
          | Content (TFn (in', lset, out')) ->
              let in' = go in' in
              let lset = go lset in
              let out' = go out' in
              Content (TFn (in', lset, out'))
          | Content (TTag tags) ->
              let go_tag (tag, args) = (tag, List.map go args) in
              let tags = List.map go_tag tags in
              Content (TTag tags)
          | Content (TRecord fields) ->
              let go_field (field, ty) = (field, go ty) in
              let fields = List.map go_field fields in
              Content (TRecord fields)
          | Content (TPrim p) -> Content (TPrim p)
        in
        tvar_set tvar ty;
        tvar
  in
  go ty
