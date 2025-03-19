open Ast
open Util
module S = Syntax.Ast

let pp_symbol f symbol =
  Format.pp_print_string f (Symbol.show_symbol_raw symbol)

let pp_typed_symbol f (_, symbol) = Format.fprintf f "%a" pp_symbol symbol

let pp_typed_symbol_t f (t, symbol) =
  Format.fprintf f "@[<hov 2>%a:@ %a@]" pp_symbol symbol Type_print.pp_ty t

let pp_pat f (p : e_pat) =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, atom) =
    match atom with
    | PTag (t, atoms) ->
        fprintf f "@[<hv 2>";
        let printer () =
          fprintf f "%s" t;
          List.iter
            (fun p ->
              fprintf f "@ ";
              go `Apply p)
            atoms
        in
        with_parens f (parens >> `Free) printer;
        fprintf f "@]"
    | PVar x -> pp_symbol f x
  in
  go `Free p

let pp_arrow f (lam, captures) =
  let open Format in
  match captures with
  | [] -> fprintf f "@[-[%a]->@]" pp_symbol lam
  | _ ->
      fprintf f "@[-[%a %a]->@]" pp_symbol lam
        (Format.pp_print_list ~pp_sep:pp_print_space pp_typed_symbol)
        captures

let rec pp_expr f =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens f (_, e) =
    match e with
    | Var x -> pp_symbol f x
    | Int i -> pp_print_int f i
    | Str s -> fprintf f "\"%s\"" (String.escaped s)
    | Unit -> pp_print_string f "{}"
    | Tag (tag, payloads) ->
        fprintf f "@[<v 0>";
        let expr () =
          fprintf f "@[<hv 2>%s%a@]" tag
            (pp_print_list (fun f e -> fprintf f "@ %a" (go `Apply) e))
            payloads
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Record fields ->
        fprintf f "@[<hv 2>{@,%a@,}@]"
          (pp_print_list ~pp_sep:Util.comma_sep (fun f (field, t) ->
               fprintf f "%s:@ %a" field (go `Free) t))
          fields
    | Access (e, field) -> fprintf f "@[<hv 2>%a.%s@]" (go `Free) e field
    | Let ((t, x), body, rest) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<v 0>@[<hov 2>let %a: %a =@ %a@]@]@ in@]@,%a" pp_symbol x
            Type_print.pp_ty t pp_expr body (go `Free) rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Call (head, args) ->
        fprintf f "@[";
        let expr () =
          fprintf f "@[<hv 2>%a(%a)@]" pp_symbol head
            (Format.pp_print_list ~pp_sep:comma_sep pp_expr)
            args
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | PackedFn { lambda; captures = Some captures } ->
        fprintf f "@[<hv 2>PackedFn(%a, %a)@]" pp_symbol lambda pp_expr captures
    | PackedFn { lambda; captures = None } ->
        fprintf f "@[<hv 2>PackedFn(%a)@]" pp_symbol lambda
    | CallIndirect (head, args) ->
        fprintf f "@[";
        let expr () =
          fprintf f "@[<hv 2>%a(%a)@]" pp_expr head
            (Format.pp_print_list ~pp_sep:comma_sep pp_expr)
            args
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | KCall (head, args) ->
        fprintf f "@[<hv 2>~%s@ %a@]"
          (List.assoc head S.string_of_kernelfn)
          (pp_print_list ~pp_sep:comma_sep (go `Apply))
          args
    | When (e, branches) ->
        fprintf f "@[<v 0>@[<v 2>when %a is%a@]@,end@]" (go `Free) e
          (pp_print_list
             ~pp_sep:(fun _ () -> ())
             (fun f (pat, body) ->
               fprintf f "@,@[<hv 2>| %a ->@ %a@]" pp_pat pat (go `Free) body))
          branches
  in
  go `Free f

let pp_captures f =
  let open Format in
  function
  | [] -> ()
  | captures ->
      fprintf f "@[<hv 2>(%a)@]"
        (pp_print_list ~pp_sep:pp_print_space pp_typed_symbol_t)
        captures

let pp_args f =
  let open Format in
  function
  | [] -> ()
  | args ->
      fprintf f "@[<hv 2>%a@]"
        (pp_print_list ~pp_sep:comma_sep pp_typed_symbol_t)
        args

let pp_def : Format.formatter -> def -> unit =
 fun f (x, def) ->
  let open Format in
  match def with
  | `Fn { args; body } ->
      fprintf f "@[<v 0>@[<v 2>fn %a(%a): %a =@ %a@]@]" pp_symbol x pp_args args
        Type_print.pp_ty (fst body) pp_expr body
  | `Val body ->
      fprintf f "@[<v 0>@[<v 2>let %a: %a =@ %a@]@]" pp_symbol x
        Type_print.pp_ty (fst body) pp_expr body
  | `Run (body, _) ->
      fprintf f "@[<v 0>@[<v 2>run %a: %a =@ %a@]@]" pp_symbol x
        Type_print.pp_ty (fst body) pp_expr body

let pp_defs : Format.formatter -> def list -> unit =
 fun f defs ->
  let open Format in
  fprintf f "@[<v 0>";
  List.iteri
    (fun i def ->
      if i > 0 then fprintf f "@,";
      pp_def f def)
    defs;
  fprintf f "@]"

let show_expr e = with_buffer (fun f -> pp_expr f e) default_width

let string_of_program ?(width = default_width) (program : program) =
  with_buffer (fun f -> pp_defs f program) width
