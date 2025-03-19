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
  | Unit
  | Tag of string * e_expr list
  | Record of (string * e_expr) list
  | Access of e_expr * string
  | Let of typed_symbol * e_expr * e_expr
  | Call of symbol * e_expr list
  | PackedFn of packed_fn
  | CallIndirect of e_expr * e_expr list
  | KCall of kernelfn * e_expr list
  | When of e_expr * branch list

and packed_fn = { lambda : symbol; captures : e_expr option }
and branch = e_pat * e_expr

type fn = { args : typed_symbol list; body : e_expr }
type def_val = [ `Fn of fn | `Val of e_expr | `Run of e_expr * Syntax.Type.tvar ]
type def = symbol * def_val
type program = def list
