open Ast
open Util
module S = Syntax.Ast

let pp_symbol f symbol =
  Format.pp_print_string f (Symbol.show_symbol_raw symbol)

let pp_typed_symbol f (_, symbol) = Format.fprintf f "%a" pp_symbol symbol

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
        fprintf f "@[<v 0>@[<hv 2>{";
        List.iteri
          (fun i (field, e) ->
            if i > 0 then fprintf f ",@ ";
            fprintf f "%s:@ " field;
            go `Free e)
          fields;
        fprintf f "}@]@]"
    | Access (e, field) ->
        fprintf f "@[<hv 2>";
        go `Apply e;
        fprintf f "@ .%s@]" field
    | Let (def, rest) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          pp_letdef f def;
          fprintf f "@ in@]@,";
          go `Free rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Clos { arg = _, x; body = e; _ } ->
        fprintf f "@[<hv 2>\\%a ->@ " pp_symbol x;
        go `Apply e;
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

and pp_letdef f = function
  | `Letfn letfn -> pp_letfn f letfn
  | `Letval letval -> pp_letval f letval

and pp_letfn f (Letfn { bind = t, x; arg; body; recursive }) =
  let open Format in
  fprintf f "@[<v 0>@[<v 2>let%s %a: %a = \\%a ->@ %a@]@]"
    (if recursive then " rec" else "")
    pp_symbol x Type_print.pp_ty t pp_symbol (snd arg) pp_expr body

and pp_letval f (Letval { bind; body; _ }) =
  let open Format in
  fprintf f "@[<v 0>@[<v 2>let %a: %a =@ %a@]@]" pp_symbol (snd bind)
    Type_print.pp_ty (fst bind) pp_expr body

let pp_def : Format.formatter -> def -> unit =
 fun f def ->
  let open Format in
  match def with
  | `Def letdef -> pp_letdef f letdef
  | `Run (Run { bind; body; _ }) ->
      fprintf f "@[<v 0>@[<v 2>run %a: %a =@ %a@]@]" pp_symbol (snd bind)
        Type_print.pp_ty (fst bind) pp_expr body

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
