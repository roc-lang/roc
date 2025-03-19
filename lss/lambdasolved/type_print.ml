open Type
open Util
open Symbol

let pp_symbol f symbol =
  Format.pp_print_string f (Symbol.show_symbol_raw symbol)

let int_of_parens_ctx = function `Free -> 1 | `AppHead -> 2 | `FnHead -> 3
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let pp_named f var c =
  let (`Var i) = var in
  Format.fprintf f "<%c%d>" c i

let pp_ty f (t : tvar) =
  let open Format in
  let rec go_lambda : variable list -> symbol -> captures -> unit =
   fun visited lambda captures ->
    fprintf f "@[<hov 2>%a" pp_symbol lambda;
    let captures = Symbol.SymbolMap.bindings captures in
    List.iter
      (fun (x, t) ->
        fprintf f "@ (%a: " pp_symbol x;
        (go visited `AppHead) f t;
        fprintf f ")")
      captures;
    fprintf f "@]"
  and go visited ctx f (t : tvar) =
    let t = unlink t in
    let var = tvar_v t in
    if List.mem var visited then
      (* This is a recursive type *)
      fprintf f "@[<v 0><rec>@]"
    else
      let visited = var :: visited in
      match tvar_deref t with
      | Unbd -> pp_named f var '?'
      | ForA -> pp_named f var '\''
      | Link t -> go visited `Free f t
      | Content (LSet lambdas) ->
          fprintf f "@[<hv 2>[@,";
          let lambdas = Symbol.SymbolMap.bindings lambdas in
          List.iteri
            (fun i (lambda, captures) ->
              go_lambda visited lambda captures;
              if i < List.length lambdas - 1 then fprintf f ",@ ")
            lambdas;
          fprintf f "@,]@]"
      | Content (TFn (a, tvar, b)) ->
          fprintf f "@[<hov 2>";
          let pty () =
            fprintf f "%a@ -@[%a@]-> %a" (go visited `Free) a (go visited `Free)
              tvar (go visited `Free) b
          in
          with_parens f (ctx >> `Free) pty;
          fprintf f "@]"
      | Content (TTag tags) ->
          fprintf f "@[<hv 2>[@,";
          pp_print_list
            ~pp_sep:(fun f () -> fprintf f ",@ ")
            (go_tag visited) f tags;
          fprintf f "@,]@]"
      | Content (TRecord fields) ->
          fprintf f "@[<hv 2>{@,";
          pp_print_list
            ~pp_sep:(fun f () -> fprintf f ",@ ")
            (fun f (field, ty) ->
              fprintf f "@[<hov 2>%s: %a@]" field (go visited `AppHead) ty)
            f fields;
          fprintf f "@,}@]"
      | Content (TPrim `Str) -> pp_print_string f "Str"
      | Content (TPrim `Int) -> pp_print_string f "Int"
      | Content (TPrim `Erased) -> pp_print_string f "Erased"
  and go_tag visited f ((tag_name, payloads) : ty_tag) =
    fprintf f "@[<hov 2>%s" tag_name;
    List.iter (fun p -> fprintf f "@ %a" (go visited `AppHead) p) payloads;
    fprintf f "@]"
  in
  go [] `Free f t

let pp_ty_top f ty = pp_ty f ty
let show_tvar tvar = Format.asprintf "%a" pp_ty tvar
let show_ty ty = Format.asprintf "%a" pp_ty ty
