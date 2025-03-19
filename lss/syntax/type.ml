open Symbol

type loc = Language.loc
type loc_symbol = loc * symbol
type variable = [ `Var of int ] [@@deriving show]

type loc_tvar = loc * tvar
and ty_tag = string * loc_tvar list
and ty_field = string * loc_tvar

(** Concrete type content *)
and ty_content =
  | TFn of loc_tvar * loc_tvar
  | TTag of { tags : ty_tag list; ext : loc_tvar }
  | TTagEmpty
  | TRecord of { fields : ty_field list; ext : loc_tvar }
  | TRecordEmpty
  | TPrim of [ `Str | `Int | `Erased ]

and ty_alias_content = { alias : loc_symbol * loc_tvar list; real : tvar }

and ty =
  | Link of tvar  (** Link to a type *)
  | Unbd of string option
  | ForA of string option  (** generalized type *)
  | Content of ty_content
  | Alias of ty_alias_content

and tvar = { ty : ty ref; var : variable }
(** Mutable type cell *)

let tvar_deref tvar = !(tvar.ty)
let tvar_set tvar ty = tvar.ty := ty
let tvar_v tvar = tvar.var

let next_var =
  let n = ref 0 in
  fun () ->
    let v = !n in
    incr n;
    `Var v

let tvar_int () = { ty = ref (Content (TPrim `Int)); var = next_var () }
let tvar_str () = { ty = ref (Content (TPrim `Str)); var = next_var () }
let tvar_erased () = { ty = ref (Content (TPrim `Erased)); var = next_var () }
let tvar_gen1 () = { ty = ref (ForA None); var = next_var () }
let min_var = 1000

let rec unlink tvar =
  match tvar_deref tvar with Link t -> unlink t | _ -> tvar

let rec unlink_w_alias tvar =
  match tvar_deref tvar with
  | Link t -> unlink_w_alias t
  | Alias { real; _ } -> unlink_w_alias real
  | _ -> tvar

let chase_tags tags ext : ty_tag list * tvar =
  let rec go : ty_tag list -> tvar -> _ =
   fun all_tags ext ->
    match tvar_deref @@ unlink ext with
    | Link _ -> failwith "unreachable"
    | Unbd _ -> (all_tags, ext)
    | ForA _ -> (all_tags, ext)
    | Content TTagEmpty -> (all_tags, ext)
    | Content (TTag { tags; ext }) -> go (all_tags @ tags) (snd ext)
    | Content (TFn _) -> failwith "not a tag"
    | Content (TPrim _) -> failwith "not a tag"
    | Content (TRecord _) -> failwith "not a tag"
    | Content TRecordEmpty -> failwith "not a tag"
    | Alias { real; _ } -> go all_tags real
  in
  go tags ext

let chase_fields fields ext : ty_field list * tvar =
  let rec go : ty_field list -> tvar -> _ =
   fun all_fields ext ->
    match tvar_deref @@ unlink ext with
    | Link _ -> failwith "unreachable"
    | Unbd _ -> (all_fields, ext)
    | ForA _ -> (all_fields, ext)
    | Content TTagEmpty -> failwith "not a record"
    | Content (TTag _) -> failwith "not a record"
    | Content (TFn _) -> failwith "not a record"
    | Content (TPrim _) -> failwith "not a record"
    | Content (TRecord { fields; ext }) -> go (all_fields @ fields) (snd ext)
    | Content TRecordEmpty -> (all_fields, ext)
    | Alias { real; _ } -> go all_fields real
  in
  go fields ext

type fresh_tvar = ty -> tvar

let is_empty_tag : tvar -> bool =
 fun t ->
  match tvar_deref @@ unlink t with
  | Content (TTag { tags = []; _ }) | Content TTagEmpty -> true
  | _ -> false
