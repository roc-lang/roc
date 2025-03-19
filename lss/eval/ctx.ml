open Symbol
module Ir = Ir.Ast

type t = { fns : (symbol * Ir.fn) list }

let make (program : Ir.program) =
  let fns =
    List.filter_map
      (function
        | def -> ( match def with Ir.Fn fn -> Some (fn.name, fn) | _ -> None))
      program
  in
  { fns }

let get_fn t name = List.assoc name t.fns
