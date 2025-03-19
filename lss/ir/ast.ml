open Layout
open Symbol
module C = Canonical_solved.Ast

type var = layout * symbol
type lit = [ `Int of int | `String of string ]
type kernelfn = C.kernelfn

type expr =
  | Var of var
  | Lit of lit
  | FnPtr of symbol
  | NullPtr
  | MakeUnion of int * var
  | GetUnionId of var
  | GetUnionStruct of var
  | MakeStruct of var list
  | GetStructField of var * int
  | CallDirect of symbol * var list
  | CallIndirect of var * var list
  | CallKFn of kernelfn * var list

type stmt =
  | Let of var * expr
  | Switch of {
      cond : var;
      branches : (int * (stmt list * expr)) list;
      join : var;
    }

type global = {
  name : symbol;
  layout : layout;
  init : expr;
  entry_ty : Syntax.Type.tvar option;
}

type fn = { name : symbol; args : var list; body : stmt list; ret : var }
type def = Global of global | Fn of fn
type program = def list
