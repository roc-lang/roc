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

  let rec go parens (_, e) =
    match e with
    | Var x -> pp_symbol f x
    | Int i -> pp_print_int f i
    | Str s -> fprintf f "\"%s\"" (String.escaped s)
    | Tag (tag, payloads) ->
        fprintf f "@[<v 0>";
        let expr () =
          fprintf f "@[<hv 2>%s@ " tag;
          List.iteri
            (fun i p ->
              if i > 0 then fprintf f "@ ";
              go `Apply p)
            payloads;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Record fields ->
        fprintf f "@[<v 0>@[<hv 2>{%a}@]@]"
          (Format.pp_print_list ~pp_sep:comma_sep (fun f (field, e) ->
               fprintf f "@[<hv 2>%s:@ %a@]" field pp_expr e))
          fields
    | Access (e, field) -> fprintf f "@[<hv 2>%a.%s@]" pp_expr e field
    | Let ((t, x), body, rest) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<v 0>@[<v 2>let %a: %a =@ %a@]@]" pp_symbol x
            Type_print.pp_ty t pp_expr body;
          fprintf f "@ in@]@,";
          go `Free rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Call (head, arg) ->
        fprintf f "@[";
        let expr () =
          fprintf f "@[<hv 2>";
          go `Apply head;
          fprintf f "@ ";
          go `Apply arg;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | KCall (head, args) ->
        fprintf f "@[<hv 2>~%s@ " (List.assoc head S.string_of_kernelfn);
        List.iteri
          (fun i arg ->
            if i > 0 then fprintf f "@ ";
            go `Apply arg)
          args;
        fprintf f "@]"
    | When (e, branches) ->
        fprintf f "@[<v 0>@[<v 2>when ";
        go `Free e;
        fprintf f " is";
        List.iteri
          (fun _i (pat, body) ->
            fprintf f "@ @[<hv 2>| %a ->@ " pp_pat pat;
            go `Free body;
            fprintf f "@]")
          branches;
        fprintf f "@]@,end@]"
  in
  go `Free

let pp_captures f =
  let open Format in
  function
  | [] -> ()
  | captures ->
      fprintf f "@[<hv 2>(%a)@]"
        (pp_print_list ~pp_sep:pp_print_space pp_typed_symbol_t)
        captures

let pp_def : Format.formatter -> def -> unit =
 fun f ((t, x), def) ->
  let open Format in
  match def with
  | `Fn { arg = _, a; captures; body; _ } ->
      fprintf f "@[<v 0>@[<v 2>let %a%a: %a = \\%a ->@ %a@]@]" pp_symbol x
        pp_captures captures Type_print.pp_ty t pp_symbol a pp_expr body
  | `Val body ->
      fprintf f "@[<v 0>@[<v 2>let %a: %a =@ %a@]@]" pp_symbol x
        Type_print.pp_ty t pp_expr body
  | `Run (body, _) ->
      fprintf f "@[<v 0>@[<v 2>run %a: %a =@ %a@]@]" pp_symbol x
        Type_print.pp_ty t pp_expr body

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

let string_of_program ?(width = default_width) (program : program) =
  with_buffer (fun f -> pp_defs f program) width
