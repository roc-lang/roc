open Memory
open Runtime
open Syntax.Type
open Syntax.Ast

let readback : Symbol.t -> memory_cell -> tvar -> e_expr =
 fun _symbols cell tvar ->
  let open Syntax in
  let open Type in
  let rec go cell t =
    let t = unlink_w_alias t in
    let expr =
      match tvar_deref t with
      | Link _ -> failwith "link after unlink"
      | Unbd _ -> Var (Symbol.unsafe_from_string "<unbound>")
      | ForA _ -> failwith "forA after monomorphization"
      | Content (TPrim `Int) -> Int (get_word cell)
      | Content (TPrim `Str) -> Str (get_string cell)
      | Content (TPrim `Erased) -> Var (Symbol.unsafe_from_string "<opaque>")
      | Content TTagEmpty -> Var (Symbol.unsafe_from_string "<void>")
      | Content (TTag { tags; ext = _, ext }) ->
          let tags, _ext = chase_tags tags ext in

          let block = get_block cell in
          let tag_id = get_word @@ List.hd block in
          let tag_struct = List.tl block in

          let tag_name, tag_payload_vars = List.nth tags tag_id in
          let tag_payload_vars = List.map snd tag_payload_vars in

          let tag_payloads = List.map2 go tag_struct tag_payload_vars in
          Tag (tag_name, tag_payloads)
      | Content TRecordEmpty -> Record []
      | Content (TRecord { fields; ext = _, ext }) ->
          let go_field (f, (_, t)) cell =
            let e = go cell t in
            (f, e)
          in
          let fields, _ext = chase_fields fields ext in
          let fields = List.map2 go_field fields (get_block cell) in
          Record fields
      | Content (TFn _) -> Var (Symbol.unsafe_from_string "<fn>")
      | Alias _ -> failwith "alias after unlink"
    in
    (Language.noloc, t, expr)
  in
  go cell tvar

let rec pp_memory_cell f = function
  | Word i -> Format.fprintf f "%d" i
  | Block l ->
      Format.fprintf f "@[[%a]@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_memory_cell)
        l
  | Label s -> Format.fprintf f "%s" (Symbol.norm_of s)

let pp_readback symbols f (cell, tvar) =
  let expr = readback symbols cell tvar in
  Format.fprintf f "@[%a@]" (Syntax.Ast_print.pp_e_expr symbols) expr

let pp_evaled symbols f { symbol; cell; ty } =
  Format.fprintf f "@[%s @[<v 0>= %a@,> %a@]@]" (Symbol.norm_of symbol)
    pp_memory_cell cell (pp_readback symbols) (cell, ty)

let pp_evaled_list f symbols (l : evaled list) =
  Format.fprintf f "@[<v 0>%a@]" (Format.pp_print_list (pp_evaled symbols)) l

let string_of_evaled ?(width = Util.default_width) symbols (list : evaled list)
    =
  Util.with_buffer (fun f -> pp_evaled_list f symbols list) width
