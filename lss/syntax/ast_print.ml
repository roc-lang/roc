open Ast
open Symbol
open Util
open Type

let pp_pat symbols f p =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, _, atom) =
    match atom with
    | PTag ((_, t), atoms) ->
        fprintf f "@[<hov 2>";
        let printer () =
          fprintf f "%s" t;
          List.iteri
            (fun i p ->
              if i > 0 then fprintf f "@ ";
              go `Apply p)
            atoms
        in
        with_parens f (parens >> `Free) printer;
        fprintf f "@]"
    | PVar (_, x) -> pp_symbol symbols f x
  in
  go `Free p

let pp_expr symbols f =
  let open Format in
  let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2 in
  let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2 in

  let rec go parens (_, _, e) =
    match e with
    | Var x -> pp_symbol symbols f x
    | Int i -> pp_print_int f i
    | Str s -> fprintf f "\"%s\"" (String.escaped s)
    | Tag (tag, payloads) ->
        fprintf f "@[<v 0>";
        let expr () =
          fprintf f "@[<hov 2>%s@ " tag;
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
          (fun i (label, e) ->
            if i > 0 then fprintf f ",@ ";
            fprintf f "@[<hov 2>%s:@ " label;
            go `Free e;
            fprintf f "@]")
          fields;
        fprintf f "@,}@]@]"
    | Access (e, label) ->
        fprintf f "@[<hov 2>";
        go `Apply e;
        fprintf f "@ .%s@]" label
    | Let { recursive = _; bind = _, _, x; expr = rhs; body } ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let %a =@ " (pp_symbol symbols) x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
          go `Free body
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Clos { arg = _, _, x; body = e } ->
        fprintf f "@[<hov 2>\\%a ->@ " (pp_symbol symbols) x;
        go `Apply e;
        fprintf f "@]"
    | Call (head, arg) ->
        fprintf f "@[<hov 2>";
        go `Apply head;
        fprintf f "@ ";
        go `Apply arg;
        fprintf f "@]"
    | KCall (head, args) ->
        fprintf f "@[<hov 2>%s@ " (List.assoc head string_of_kernelfn);
        List.iteri
          (fun i arg ->
            if i > 0 then fprintf f "@ ";
            go `Apply arg)
          args;
        fprintf f "@]"
    | When (e, branches) ->
        fprintf f "@[<v 0>@[<hv 2>when@ ";
        go `Free e;
        fprintf f " is@]@ @[<hv 2>";
        List.iteri
          (fun i (pat, body) ->
            fprintf f "|@ %a ->@ " (pp_pat symbols) pat;
            go `Free body;
            if i < List.length branches - 1 then fprintf f "@ ")
          branches;
        fprintf f "@]@,@]"
  in
  go `Free

let pp_e_expr = pp_expr

let string_of_expr symbols e =
  with_buffer (fun f -> pp_expr symbols f e) default_width

let pp_def : Symbol.t -> Format.formatter -> e_def -> unit =
 fun symbols f (_, tvar, def) ->
  let open Format in
  match def with
  | TyAlias ((_, x), args, (_, ty)) ->
      fprintf f "@[<hov 2>@[<hv 2>%a" (pp_symbol symbols) x;
      let names = Type_print.name_vars @@ List.map snd args @ [ ty ] in
      List.iter
        (fun (_, ty) ->
          fprintf f " ";
          Type_print.pp_tvar symbols [] names f ty)
        args;
      fprintf f "@]@ :@ ";
      Type_print.pp_tvar symbols [ tvar_v tvar ] names f ty;
      fprintf f "@]"
  | Sig ((_, x), ty) ->
      let names = Type_print.name_vars [ snd ty ] in
      fprintf f "@[<hov 2>@[<hv 2>sig %a :@ " (pp_symbol symbols) x;
      Type_print.pp_tvar symbols [] names f @@ snd ty;
      fprintf f "@]@]"
  | Def ((_, x), e) ->
      fprintf f "@[<hov 2>@[<hv 2>let %a =@ " (pp_symbol symbols) x;
      pp_expr symbols f e;
      fprintf f "@]@]"
  | Run ((_, x), e) ->
      fprintf f "@[<hov 2>@[<hv 2>run %a =@ " (pp_symbol symbols) x;
      pp_expr symbols f e;
      fprintf f "@]@]"

let pp_defs : Symbol.t -> Format.formatter -> e_def list -> unit =
 fun symbols f defs ->
  let open Format in
  fprintf f "@[<v 0>";
  let rec go : e_def list -> unit = function
    | [] -> ()
    | [ def ] -> pp_def symbols f def
    | ((_, _, Sig ((_, x), _)) as sig_)
      :: ((_, _, (Def ((_, y), _) | Run ((_, y), _))) :: _ as defs)
      when x = y ->
        pp_def symbols f sig_;
        fprintf f "@,";
        go defs
    | def :: defs ->
        pp_def symbols f def;
        fprintf f "@,@,";
        go defs
  in
  go defs;
  fprintf f "@]"

let string_of_program ?(width = default_width) (symbols : Symbol.t)
    (program : program) =
  with_buffer (fun f -> pp_defs symbols f program) width
