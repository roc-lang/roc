open Canonical_solved.Type

type ty_cache = (variable * tvar) list ref

let clone_inst : fresh_tvar -> ty_cache -> tvar -> tvar =
 fun fresh_tvar cache tvar ->
  let rec go_loc : loc_tvar -> loc_tvar = fun (l, t) -> (l, go t)
  and go : tvar -> tvar =
   fun tvar ->
    let { var; ty } = unlink tvar in
    match List.assoc_opt var !cache with
    | Some tvar -> tvar
    | None ->
        let tvar = fresh_tvar @@ Unbd None in
        cache := (var, tvar) :: !cache;

        let ty =
          match !ty with
          | Link _ -> failwith "clone_type: Link"
          | Unbd x -> Unbd x
          | ForA x -> Unbd x
          | Content (TPrim p) -> Content (TPrim p)
          | Content (TTag { tags; ext }) ->
              let go_tag (tag, args) = (tag, List.map go_loc args) in
              let tags = List.map go_tag tags in
              let ext = go_loc ext in
              Content (TTag { tags; ext })
          | Content TTagEmpty -> Content TTagEmpty
          | Content (TRecord { fields; ext }) ->
              let go_field (field, ty) = (field, go_loc ty) in
              let fields = List.map go_field fields in
              let ext = go_loc ext in
              Content (TRecord { fields; ext })
          | Content TRecordEmpty -> Content TRecordEmpty
          | Content (TFn (in', out)) ->
              let in' = go_loc in' in
              let out = go_loc out in
              Content (TFn (in', out))
          | Alias { alias = sym, args; real } ->
              let args = List.map go_loc args in
              let real = go real in
              Alias { alias = (sym, args); real }
        in
        tvar_set tvar ty;
        tvar
  in
  go tvar
