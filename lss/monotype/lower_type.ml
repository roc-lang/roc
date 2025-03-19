open Type
module T = Canonical_solved.Type

type mono_cache = (T.variable * ty) list ref

let fresh_mono_cache () : mono_cache = ref []

let unlink_tvar tvar =
  let rec go tvar =
    match T.tvar_deref tvar with
    | Link tvar -> go tvar
    | Alias { real; _ } -> go real
    | _ -> tvar
  in
  go tvar

let ty_unfilled () = TTag [ ("__unfilled", []) ]

let lower_type : mono_cache -> T.tvar -> ty =
 fun cache tvar ->
  let fail s =
    failwith
      (Printf.sprintf "lower_type: %s: %s" s (Syntax.Type_print.show_tvar tvar))
  in
  let rec go_content : T.ty_content -> ty_content = function
    | T.TFn ((_, in'), (_, out)) ->
        let in' = go in' in
        let out = go out in
        TFn (in', out)
    | T.TTag { tags; ext = _, ext } ->
        let tags, ext = T.chase_tags tags ext in
        let tags = Util.sort_tagged @@ List.map go_tag tags in
        assert (!(go ext) = TTag []);
        TTag tags
    | T.TTagEmpty -> TTag []
    | T.TRecord { fields; ext = _, ext } ->
        let fields, ext = T.chase_fields fields ext in
        let fields = Util.sort_tagged @@ List.map go_field fields in
        assert (!(go ext) = TTag [] || !(go ext) = TRecord []);
        TRecord fields
    | T.TRecordEmpty -> TRecord []
    | T.TPrim p -> TPrim p
  and go_tag : T.ty_tag -> ty_tag =
   fun (tag, args) ->
    let args = List.map (fun (_, t) -> go t) args in
    (tag, args)
  and go_field : T.ty_field -> ty_field =
   fun (field, (_, t)) ->
    let t = go t in
    (field, t)
  and go tvar =
    let tvar = unlink_tvar tvar in
    let var = T.tvar_v tvar in
    match List.assoc_opt var !cache with
    | Some ty -> ty
    | None ->
        let ty = ref (ty_unfilled ()) in
        cache := (var, ty) :: !cache;
        let content =
          match T.tvar_deref tvar with
          | T.Link _ -> fail "unexpected link"
          | T.Unbd _ -> TTag []
          | T.ForA _ -> fail "unexpected generalized type"
          | T.Content c -> go_content c
          | T.Alias _ -> fail "unexpected alias"
        in
        ty := content;
        ty
  in
  go tvar
