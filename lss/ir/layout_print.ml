open Layout

type rec_name = [ `Rec of string ]

let int_to_rec_name : int -> rec_name =
 fun i ->
  let s = List.nth [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] i in
  `Rec s

let pp_rec : Format.formatter -> rec_name -> unit =
 fun f (`Rec s) -> Format.fprintf f "!%s" s

let pp_layout : Format.formatter -> layout -> unit =
 fun f l ->
  let open Format in
  let rec go visited f l =
    match List.find_opt (fun (l', _) -> l == l') visited with
    | Some (_, i) -> fprintf f "%a" pp_rec i
    | None -> (
        match !l with
        | Str -> fprintf f "str"
        | Int -> fprintf f "int"
        | OpaquePtr -> fprintf f "*opaque"
        | Struct [] -> fprintf f "{}"
        | Struct layouts ->
            (* format as { lay1, lay2, lay3 } *)
            (* or
               {
                 lay1,
                 lay2,
                 lay3,
               }
               if a break is required
            *)
            fprintf f "@[<hv 0>{@[<hv 0>@ %a@ @]%t}@]"
              (pp_print_list
                 ~pp_sep:(fun f () -> fprintf f ",@, ")
                 (go visited))
              layouts
              (pp_print_custom_break ~fits:("", 0, "") ~breaks:(",", 0, ""))
        | Union [] -> fprintf f "[]"
        | Union variants ->
            (* format as [ lay1, lay2, lay3 ] *)
            (* or
               [
                 lay1,
                 lay2,
                 lay3,
               ]
               if a break is required
            *)
            fprintf f "@[<hv 0>[@[<hv 2>";
            List.iteri
              (fun i lay ->
                fprintf f "@ `%d %a" i (go visited) lay;
                if i < List.length variants - 1 then fprintf f ",")
              variants;
            fprintf f "@]@ ]@]"
        | Box inner ->
            let rn = int_to_rec_name @@ List.length visited in
            let visited = (l, rn) :: visited in
            fprintf f "@[<hv 0>Box%a(%a)@]" pp_rec rn (go visited) inner
        | INTERNAL__Unfilled -> fprintf f "??Unfilled")
  in
  go [] f l

let show_layout = Format.asprintf "%a" pp_layout
