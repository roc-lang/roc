open Type

let int_of_parens_ctx = function `Free -> 1 | `AppHead -> 2 | `FnHead -> 3
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let pp_ty : Format.formatter -> tvar -> unit =
  let open Format in
  fun f ty ->
    let rec go_tag visited f (tag_name, payloads) =
      fprintf f "@[<hov 2>%s" tag_name;
      List.iter (fun p -> fprintf f "@ %a" (go visited) p) payloads;
      fprintf f "@]"
    and go visited f ty =
      let var = tvar_v ty in
      if List.mem var visited then
        (* This is a recursive type *)
        fprintf f "@[<v 0><rec>@]"
      else
        let visited = var :: visited in
        match tvar_deref ty with
        | TTag tags ->
            fprintf f "@[<hv 2>[@,%a@,]@]"
              (pp_print_list
                 ~pp_sep:(fun f () -> fprintf f ",@ ")
                 (go_tag visited))
              tags
        | TRecord bindings ->
            fprintf f "@[<hv 2>{@,%a@,}@]"
              (pp_print_list
                 ~pp_sep:(fun f () -> fprintf f ",@ ")
                 (fun f (name, ty) ->
                   fprintf f "@[<hov 2>%s: %a@]" name (go visited) ty))
              bindings
        | TPrim `Str -> pp_print_string f "Str"
        | TPrim `Int -> pp_print_string f "Int"
        | TPrim `Erased -> pp_print_string f "Erased"
    in
    go [] f ty

let show_ty ty = Format.asprintf "%a" pp_ty ty
