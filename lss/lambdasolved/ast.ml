open Type
open Symbol
module C = Canonical_solved.Ast

type typed_symbol = tvar * symbol
type kernelfn = C.kernelfn
type kernel_sig = C.kernel_sig

let kernel_sig = C.kernel_sig

type e_pat = tvar * pat
and pat = PTag of string * e_pat list | PVar of symbol

type e_expr = tvar * expr

and expr =
  | Var of symbol
  | Int of int
  | Str of string
  | Tag of string * e_expr list
  | Record of (string * e_expr) list
  | Access of e_expr * string
  | Let of typed_symbol * e_expr * e_expr
  | Call of e_expr * e_expr
  | KCall of kernelfn * e_expr list
  | When of e_expr * branch list

and branch = e_pat * e_expr

type captures = tvar SymbolMap.t
type fn = { arg : typed_symbol; captures : captures; body : e_expr }
type def_val = [ `Fn of fn | `Val of e_expr | `Run of e_expr * Syntax.Type.tvar ]
type def = typed_symbol * def_val
type program = def list
