open Layout
module T = Lambdamono.Type

type type_cache = (T.variable * layout) list ref

let new_type_cache () : type_cache = ref []
let unfilled_layout = INTERNAL__Unfilled
let unfilled_boxed_layout = Box (ref unfilled_layout)

let lower_type : type_cache -> T.tvar -> layout =
 fun cache ty ->
  let rec go ty =
    let var = T.tvar_v ty in
    match List.assoc_opt var !cache with
    | Some layout ->
        if !layout = unfilled_layout then
          (* mark the layout as needing to be boxed *)
          layout := unfilled_boxed_layout;
        layout
    | None ->
        let layout = ref @@ unfilled_layout in
        cache := (var, layout) :: !cache;
        let content =
          match T.tvar_deref ty with
          | T.TTag tags ->
              let lower_tag (_, args) =
                let struct' = List.map go args in
                ref @@ Struct struct'
              in
              Union (List.map lower_tag tags)
          | T.TRecord fields ->
              let struct' = List.map snd fields |> List.map go in
              Struct struct'
          | T.TPrim `Str -> Str
          | T.TPrim `Int -> Int
          | T.TPrim `Erased -> OpaquePtr
        in
        let content =
          if !layout = unfilled_boxed_layout then Box (ref content) else content
        in
        layout := content;
        layout
  in
  go ty
