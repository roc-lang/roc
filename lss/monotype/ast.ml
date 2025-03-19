open Type
open Symbol
module C = Canonical_solved.Ast

type typed_symbol = ty * symbol
type kernelfn = C.kernelfn
type kernel_sig = C.kernel_sig

let kernel_sig = C.kernel_sig

type e_pat = ty * pat
and pat = PTag of string * e_pat list | PVar of symbol

type e_expr = ty * expr

and letfn =
  | Letfn of {
      recursive : bool;
      bind : typed_symbol;
      arg : typed_symbol;
      body : e_expr;
    }

and letval = Letval of { bind : typed_symbol; body : e_expr }
and let_def = [ `Letval of letval | `Letfn of letfn ]

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

type run_def =
  | Run of { bind : typed_symbol; body : e_expr; ty : Syntax.Type.tvar }

type def = [ `Def of let_def | `Run of run_def ]
type program = def list

let bind_of_let_def = function
  | `Letfn (Letfn { bind; _ }) -> bind
  | `Letval (Letval { bind; _ }) -> bind
