open Symbol
open Type
module S = Syntax.Ast

type typed_symbol = tvar * symbol
type kernelfn = S.kernelfn
type kernel_sig = S.kernel_sig

let kernel_sig = S.kernel_sig

type e_pat = tvar * pat
and pat = PTag of string * e_pat list | PVar of symbol

type e_expr = tvar * expr

and letfn =
  | Letfn of {
      recursive : symbol Option.t;
      bind : typed_symbol;
      arg : typed_symbol;
      body : e_expr;
      sig_ : tvar option;
    }

and letval =
  | Letval of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

and let_def = [ `Letfn of letfn | `Letval of letval ]

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Tag of string * e_expr list
  | Record of (string * e_expr) list
  | Access of e_expr * string
  | Let of let_def * e_expr
  | Clos of { arg : typed_symbol; body : e_expr }
  | Call of e_expr * e_expr
  | KCall of kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

let name_of_letfn = function Letfn { bind = _, x; _ } -> x
let name_of_letval = function Letval { bind = _, x; _ } -> x
let type_of_letfn = function Letfn { bind = ty, _; _ } -> ty
let type_of_letval = function Letval { bind = ty, _; _ } -> ty

type run_def =
  | Run of { bind : typed_symbol; body : e_expr; sig_ : tvar option }

type def = [ `Def of let_def | `Run of run_def ]
type program = def list

let name_of_let_def = function
  | `Letfn letfn -> name_of_letfn letfn
  | `Letval letval -> name_of_letval letval

let name_of_def = function
  | `Def let_def -> name_of_let_def let_def
  | `Run (Run { bind = _, x; _ }) -> x
