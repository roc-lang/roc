type variable = [ `Var of int ] [@@deriving show]

type ty_content =
  | TTag of (string * tvar list) list
  | TRecord of (string * tvar) list
  | TPrim of [ `Str | `Int | `Erased ]

and tvar = { ty : ty_content ref; var : variable }

let tvar_v : tvar -> variable = fun { var; _ } -> var
let tvar_deref : tvar -> ty_content = fun { ty; _ } -> !ty
let tvar_set tvar ty = tvar.ty := ty
let min_var = 0

type fresh_tvar = ty_content -> tvar

let fresh_tvar_generator () =
  let next_tvar = ref min_var in
  fun ty ->
    let tvar = { ty = ref ty; var = `Var !next_tvar } in
    incr next_tvar;
    tvar

let equal_tvar : tvar -> tvar -> bool =
 fun ty1 ty2 ->
  let rec go visited ty1 ty2 =
    let v1, v2 = (tvar_v ty1, tvar_v ty2) in
    if List.mem (v1, v2) visited then true
    else if v1 == v2 then true
    else
      let visited = (v1, v2) :: visited in
      match (tvar_deref ty1, tvar_deref ty2) with
      | TTag ty_tags, TTag ty_tags' ->
          List.for_all2
            (fun (tag1, tys1) (tag2, tys2) ->
              tag1 = tag2
              && List.length tys1 = List.length tys2
              && List.for_all2 (go visited) tys1 tys2)
            ty_tags ty_tags'
      | TRecord tys, TRecord tys' ->
          List.for_all2
            (fun (field1, ty1) (field2, ty2) ->
              field1 = field2 && go visited ty1 ty2)
            tys tys'
      | TPrim `Str, TPrim `Str -> true
      | TPrim `Int, TPrim `Int -> true
      | _ -> false
  in
  go [] ty1 ty2
