open Ast
open Symbol
open Layout_print
open Util
module S = Syntax.Ast

let pp_symbol : Format.formatter -> symbol -> unit =
 fun f s -> Format.fprintf f "%s" (Symbol.norm_of s)

let pp_var : Format.formatter -> var -> unit =
 fun f (lay, symbol) ->
  Format.fprintf f "@[<hov 2>%a:@ %a@]" pp_symbol symbol pp_layout lay

let pp_v_name : Format.formatter -> var -> unit =
 fun f (_, symbol) -> pp_symbol f symbol

let pp_v_names : Format.formatter -> var list -> unit =
 fun f vs ->
  List.iteri
    (fun i (_, symbol) ->
      Format.fprintf f "%a" pp_symbol symbol;
      if i < List.length vs - 1 then Format.fprintf f ",@, ")
    vs

let pp_expr : Format.formatter -> expr -> unit =
  let open Format in
  fun f -> function
    | Var v -> fprintf f "%a" pp_v_name v
    | Lit l -> (
        match l with
        | `Int i -> fprintf f "%d" i
        | `String s -> fprintf f "\"%s\"" @@ String.escaped s)
    | FnPtr s -> fprintf f "@[<hv 2>@fn<@,%a>@]" pp_symbol s
    | NullPtr -> fprintf f "@[<hv 2>@nullptr@]"
    | MakeUnion (i, v) ->
        fprintf f "@[<hv 2>@make_union<@,%d,@ %a>@]" i pp_v_name v
    | GetUnionId v -> fprintf f "@[<hv 2>@get_union_id<@,%a>@]" pp_v_name v
    | GetUnionStruct v ->
        fprintf f "@[<hv 2>@get_union_struct<@,%a>@]" pp_v_name v
    | MakeStruct vs ->
        let pp_vs f = function
          | [] -> ()
          | vs -> fprintf f "@ %a@ " pp_v_names vs
        in
        fprintf f "@[<hov 0>@make_struct{%a%t}@]" pp_vs vs
          (pp_print_custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))
    | GetStructField (v, i) ->
        fprintf f "@[<hv 2>@get_struct_field<@,%a,@ %d>@]" pp_v_name v i
    | CallDirect (fn, args) ->
        let pp_args f = function
          | [] -> ()
          | args -> fprintf f ",@ %a" pp_v_names args
        in
        fprintf f "@[<hv 2>@call_direct(@,%a%a)@]" pp_symbol fn pp_args args
    | CallIndirect (fn, args) ->
        let pp_args f = function
          | [] -> ()
          | args -> fprintf f ",@ %a" pp_v_names args
        in
        fprintf f "@[<hv 2>@call_indirect(@,%a%a)@]" pp_v_name fn pp_args args
    | CallKFn (kfn, args) ->
        let pp_args f = function
          | [] -> ()
          | args -> fprintf f ",@ %a" pp_v_names args
        in
        fprintf f "@[<hv 2>@call_kfn(@,%s%a)@]"
          (List.assoc kfn S.string_of_kernelfn)
          pp_args args

let rec pp_stmt : Format.formatter -> stmt -> unit =
  let open Format in
  fun f -> function
    | Let (v, e) -> fprintf f "@[<hv 2>let %a@ = %a;@]" pp_var v pp_expr e
    | Switch { cond; branches; join } ->
        let pp_stmts f = function
          | [] -> ()
          | stmts ->
              fprintf f "%a@ "
                (pp_print_list ~pp_sep:pp_print_space pp_stmt)
                stmts
        in
        let pp_branch f (i, (lets, ret)) =
          fprintf f "@[<v 0>@[<v 2>%d -> {@ %a%a@]@ }@]" i pp_stmts lets pp_expr
            ret
        in
        fprintf f "@[<v 0>switch %a {@,%a@,} in join %a;@]" pp_v_name cond
          (pp_print_list pp_branch) branches pp_v_name join

let show_stmts stmts = Format.asprintf "%a" (Format.pp_print_list pp_stmt) stmts

let pp_fn : Format.formatter -> fn -> unit =
  let open Format in
  fun f { name; args; body; ret = ret_lay, ret_x } ->
    let pp_args f = function
      | [] -> ()
      | args ->
          List.iteri
            (fun i v ->
              fprintf f "%a" pp_var v;
              if i < List.length args - 1 then fprintf f ",@, ")
            args
    in

    fprintf f
      "@[<v 0>@[<hv 2>@[<hv 2>fn %a(@,%a)@]:@ %a@]@ @[<v 0>{@;<0 2>@[<v 0>"
      pp_symbol name pp_args args pp_layout ret_lay;
    List.iter (fun s -> fprintf f "%a@," pp_stmt s) body;
    fprintf f "return %a;@]@,@]}@]" pp_symbol ret_x

let pp_global : Format.formatter -> global -> unit =
  let open Format in
  fun f { name; layout; init; entry_ty } ->
    let entry = Option.fold ~none:"global" ~some:(fun _ -> "entry") entry_ty in
    fprintf f "@[<hv 2>%s %a:@ %a@ = %a;@]" entry pp_symbol name pp_layout
      layout pp_expr init

let pp_definition : Format.formatter -> def -> unit =
 fun f -> function Fn p -> pp_fn f p | Global g -> pp_global f g

let pp_program : Format.formatter -> program -> unit =
  let open Format in
  fun f definitions ->
    fprintf f "@[<v 0>%a@]"
      (pp_print_list ~pp_sep:(fun f () -> fprintf f "@,@,") pp_definition)
      definitions

let string_of_program ?(width = default_width) p =
  with_buffer (fun f -> pp_program f p) width
