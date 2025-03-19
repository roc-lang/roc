open Symbol
open Type
open Language

type e_pat = loc * tvar * pat
(** An elaborated pattern *)

and pat = PTag of (loc * string) * e_pat list | PVar of loc_symbol

type kernelfn = [ `StrConcat | `Itos | `Add | `Sub | `Erase | `Unerase ]

let string_of_kernelfn : (kernelfn * string) list =
  [
    (`StrConcat, "str_concat");
    (`Add, "add");
    (`Sub, "sub");
    (`Itos, "itos");
    (`Erase, "erase");
    (`Unerase, "unerase");
  ]

let kernelfn_of_string : (string * kernelfn) list =
  List.map (fun (a, b) -> (b, a)) string_of_kernelfn

type kernel_sig = {
  args : [ `Variadic of tvar | `List of tvar list ];
  ret : tvar;
}

let kernel_sig : kernelfn -> kernel_sig = function
  | `StrConcat -> { args = `Variadic (tvar_str ()); ret = tvar_str () }
  | `Add -> { args = `Variadic (tvar_int ()); ret = tvar_int () }
  | `Sub -> { args = `Variadic (tvar_int ()); ret = tvar_int () }
  | `Itos -> { args = `List [ tvar_int () ]; ret = tvar_str () }
  | `Erase -> { args = `List [ tvar_gen1 () ]; ret = tvar_erased () }
  | `Unerase -> { args = `List [ tvar_erased () ]; ret = tvar_gen1 () }

type e_expr = loc * tvar * expr
(** An elaborated expression *)

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Tag of string * e_expr list
  | Record of (string * e_expr) list
  | Access of e_expr * string
  | Let of {
      recursive : bool ref;
      bind : loc * loc_tvar * symbol;
      expr : e_expr;
      body : e_expr;
    }
  | Clos of { arg : loc * loc_tvar * symbol; body : e_expr }
  | Call of e_expr * e_expr
  | KCall of kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

(** A top-level definition *)
type def =
  | TyAlias of loc_symbol * loc_tvar list * loc_tvar
  | Sig of loc_symbol * loc_tvar
  | Def of loc_symbol * e_expr
  | Run of loc_symbol * e_expr

type e_def = loc * tvar * def
(** An elaborated definition *)

type program = e_def list
type parse_ctx = { fresh_tvar : fresh_tvar; symbols : Symbol.t }

let fresh_parse_ctx () : parse_ctx =
  let n = ref Type.min_var in
  let fresh_int () =
    incr n;
    !n
  in
  let fresh_tvar : Type.fresh_tvar =
   fun ty -> { ty = ref ty; var = `Var (fresh_int ()) }
  in
  let symbols = Symbol.make () in
  { fresh_tvar; symbols }

(* extractions *)
let xloc (l, _, _) = l
let xty (_, t, _) = t
let xv (_, _, v) = v
