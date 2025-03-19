open Language
open Syntax

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

type parsed_program = {
  symbols : Symbol.t;
  fresh_tvar : Syntax.Type.fresh_tvar;
  syntax : Syntax.Ast.program;
}

let parse s =
  let lexbuf = Lexer.from_string s in
  let lex = Lexer.provider lexbuf in
  let parse =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel
  in
  try
    let parse_ctx = Ast.fresh_parse_ctx () in
    let syntax = parse lex parse_ctx in
    Ok
      { symbols = parse_ctx.symbols; syntax; fresh_tvar = parse_ctx.fresh_tvar }
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position (Lexer.position lexbuf)))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position (Lexer.position lexbuf)))

type canonicalized_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  fresh_tvar : Canonical.Type.fresh_tvar;
  canonical : Canonical.Ast.program;
}

let canonicalize ({ symbols; syntax; fresh_tvar } : parsed_program) =
  try
    let canonical = Canonical.Lower.lower { symbols; fresh_tvar } syntax in
    Ok { symbols; fresh_tvar; syntax; canonical }
  with Canonical.Lower.Can_error e -> Error e

type solved_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  fresh_tvar : Canonical.Type.fresh_tvar;
  canonical_solved : Canonical_solved.Ast.program;
}

let solve ({ symbols; syntax; fresh_tvar; canonical } : canonicalized_program) =
  try
    let canonical_solved =
      Canonical_solved.Lower.lower { symbols; fresh_tvar } canonical
    in
    Ok { symbols; fresh_tvar; syntax; canonical_solved }
  with Canonical_solved.Lower.Solve_err e -> Error e

type monotype_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  monotype : Monotype.Ast.program;
}

let monotype
    ({ symbols; syntax; fresh_tvar; canonical_solved } : solved_program) =
  let ctx = Monotype.Lower.make_context ~symbols ~fresh_tvar canonical_solved in
  let monotype = Monotype.Lower.lower ctx canonical_solved in
  Ok { symbols; syntax; monotype }

type monotype_lifted_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  monotype_lifted : Monotype_lifted.Ast.program;
}

let monotype_lifted ({ symbols; syntax; monotype } : monotype_program) =
  let ctx = Monotype_lifted.Ctx.make ~symbols monotype in
  let monotype_lifted = Monotype_lifted.Lower.lower ctx monotype in
  Ok { symbols; syntax; monotype_lifted }

type lambdasolved_program = {
  symbols : Symbol.t;
  fresh_tvar : Lambdasolved.Type.fresh_tvar;
  syntax : Syntax.Ast.program;
  lambdasolved : Lambdasolved.Ast.program;
}

let lambdasolved
    ({ symbols; syntax; monotype_lifted } : monotype_lifted_program) =
  let ctx = Lambdasolved.Ctx.make () in
  let lambdasolved = Lambdasolved.Lower.lower ctx monotype_lifted in
  Ok { symbols; syntax; lambdasolved; fresh_tvar = ctx.fresh_tvar }

type lambdamono_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  lambdamono : Lambdamono.Ast.program;
}

let lambdamono
    ({ symbols; syntax; lambdasolved; fresh_tvar } : lambdasolved_program) =
  let ctx = Lambdamono.Ctx.make ~symbols ~fresh_tvar lambdasolved in
  let lambdamono = Lambdamono.Lower.lower ctx lambdasolved in
  Ok { symbols; syntax; lambdamono }

type ir_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  ir : Ir.Ast.program;
}

let ir ({ symbols; syntax; lambdamono } : lambdamono_program) =
  let ctx = Ir.Ctx.make ~symbols in
  let ir = Ir.Lower.lower ~ctx lambdamono in
  Ok { symbols; syntax; ir }

type evaled_program = {
  symbols : Symbol.t;
  syntax : Syntax.Ast.program;
  evaled : Eval.Runtime.evaled list;
}

let eval ({ symbols; syntax; ir } : ir_program) =
  let ctx = Eval.Ctx.make ir in
  let evaled = Eval.Runtime.eval ~ctx ir in
  Ok { symbols; syntax; evaled }

let ( let* ) = Result.bind

module Lss : LANGUAGE = struct
  let name = "lss"

  let run ~stage source =
    match stage with
    | "parse" ->
        let* { syntax; symbols; _ } = parse source in
        Ok (Syntax.Ast_print.string_of_program symbols syntax)
    | "canonicalize" ->
        let* p = parse source in
        let* { canonical; _ } = canonicalize p in
        Ok (Canonical.Print.string_of_program canonical)
    | "solve" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* { canonical_solved; _ } = solve p in
        Ok (Canonical_solved.Print.string_of_program canonical_solved)
    | "monotype" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* { monotype; _ } = monotype p in
        Ok (Monotype.Print.string_of_program monotype)
    | "monotype_lifted" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* { monotype_lifted; _ } = monotype_lifted p in
        Ok (Monotype_lifted.Print.string_of_program monotype_lifted)
    | "lambdasolved" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* p = monotype_lifted p in
        let* { lambdasolved; _ } = lambdasolved p in
        Ok (Lambdasolved.Print.string_of_program lambdasolved)
    | "lambdamono" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* p = monotype_lifted p in
        let* p = lambdasolved p in
        let* { lambdamono; _ } = lambdamono p in
        Ok (Lambdamono.Print.string_of_program lambdamono)
    | "ir" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* p = monotype_lifted p in
        let* p = lambdasolved p in
        let* p = lambdamono p in
        let* { ir; _ } = ir p in
        Ok (Ir.Print.string_of_program ir)
    | "eval" ->
        let* p = parse source in
        let* p = canonicalize p in
        let* p = solve p in
        let* p = monotype p in
        let* p = monotype_lifted p in
        let* p = lambdasolved p in
        let* p = lambdamono p in
        let* p = ir p in
        let* { evaled; symbols; _ } = eval p in
        Ok (Eval.Readback.string_of_evaled symbols evaled)
    | _ -> Error (Format.sprintf "Invalid stage: %s" stage)

  let type_at loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syntax; _ } = solve p in
    let ty =
      Syntax.Query.type_at loc syntax
      |> Option.map (fun ty ->
             let names = Syntax.Type_print.name_vars [ ty ] in
             Syntax.Type_print.string_of_tvar default_width symbols names ty)
    in
    Ok ty

  let hover_info loc s =
    let* p = parse s in
    let* p = canonicalize p in
    let* { symbols; syntax; _ } = solve p in
    Ok (Syntax.Query.hover_info loc syntax symbols)
end
