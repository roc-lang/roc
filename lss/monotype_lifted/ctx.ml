type t = { symbols : Symbol.t; toplevels : Symbol.symbol list ref }

let extract_toplevels (program : Monotype.Ast.program) =
  let open Monotype.Ast in
  let rec go acc = function
    | [] -> acc
    | `Run (Run { bind = _, x; _ }) :: rest
    | `Def (`Letfn (Letfn { bind = _, x; _ })) :: rest
    | `Def (`Letval (Letval { bind = _, x; _ })) :: rest ->
        go (x :: acc) rest
  in
  go [] program

let add_toplevel t x = t.toplevels := x :: !(t.toplevels)

let make ~symbols (program : Monotype.Ast.program) =
  { symbols; toplevels = ref (extract_toplevels program) }
