open Memory
open Symbol
open Ir.Ast
module S = Syntax.Ast

let word i = Word i
let block l = Block l
let label l = Label l
let get_word = function Word i -> i | _ -> failwith "not a word"
let get_block = function Block l -> l | _ -> failwith "not a block"
let get_label = function Label l -> l | _ -> failwith "not a label"

let get_string cell =
  let block = get_block cell in
  let words = List.map get_word block in
  String.of_seq @@ List.to_seq @@ List.map Char.chr words

let make_string s =
  block @@ List.map word @@ List.map Char.code @@ List.of_seq @@ String.to_seq s

let eval_kcall : S.kernelfn -> memory_cell list -> memory_cell =
 fun kfn args ->
  match kfn with
  | `StrConcat ->
      let ss = List.map get_string args in
      make_string @@ String.concat "" ss
  | `Itos ->
      let i = get_word @@ List.hd args in
      make_string @@ string_of_int i
  | `Add ->
      let is = List.map get_word args in
      word @@ List.fold_left ( + ) 0 is
  | `Sub ->
      let is = List.map get_word args in
      word @@ List.fold_left ( - ) (List.hd is) (List.tl is)
  | `Erase -> List.hd args
  | `Unerase -> List.hd args

let rec eval_expr ~ctx memory expr =
  let lookup x = List.assoc (snd x) memory in
  match expr with
  | Var x -> lookup x
  | Lit (`Int i) -> word i
  | Lit (`String s) -> make_string s
  | NullPtr -> block []
  | FnPtr fn -> label fn
  | MakeUnion (id, var) -> block @@ (word id :: get_block (lookup var))
  | GetUnionId var -> List.hd @@ get_block @@ lookup var
  | GetUnionStruct var -> block @@ List.tl @@ get_block @@ lookup var
  | MakeStruct vs -> block @@ List.map lookup vs
  | GetStructField (v, i) -> List.nth (get_block @@ lookup v) i
  | CallDirect (fn, args) ->
      let args = List.map lookup args in
      eval_call_direct ~ctx memory fn args
  | CallIndirect (fn, args) ->
      let fn = lookup fn in
      let args = List.map lookup args in
      eval_call_indirect ~ctx memory fn args
  | CallKFn (fn, args) ->
      let args = List.map lookup args in
      eval_kcall fn args

and eval_stmt ~ctx memory stmt =
  match stmt with
  | Let (x, e) ->
      let data = eval_expr ~ctx memory e in
      (snd x, data) :: memory
  | Switch { cond; branches; join } ->
      let id = get_word @@ List.assoc (snd cond) memory in
      let branch_stmts, branch_expr = List.assoc id branches in
      let memory = eval_stmts ~ctx memory branch_stmts in
      eval_stmt ~ctx memory (Let (join, branch_expr))

and eval_stmts ~ctx memory stmts = List.fold_left (eval_stmt ~ctx) memory stmts

and eval_call_direct ~ctx memory fn args =
  let { args = arg_vars; body; ret; name = _ } = Ctx.get_fn ctx fn in
  let arg_syms = List.map snd arg_vars in
  let arg_cells = List.combine arg_syms args in
  let memory = arg_cells @ memory in
  let memory = eval_stmts ~ctx memory body in
  List.assoc (snd ret) memory

and eval_call_indirect ~ctx memory fn args =
  let fn, captures =
    match get_block fn with
    | [ Label fn; Block [] ] -> (fn, [])
    | [ Label fn; captures ] -> (fn, [ captures ])
    | _ -> failwith "not a fn ptr"
  in
  let { args = arg_vars; body; ret; name = _ } = Ctx.get_fn ctx fn in
  let arg_syms = List.map snd arg_vars in
  let arg_cells = List.combine arg_syms (args @ captures) in
  let memory = arg_cells @ memory in
  let memory = eval_stmts ~ctx memory body in
  List.assoc (snd ret) memory

let eval_globals ~ctx definitions =
  let rec go memory = function
    | [] -> memory
    | Fn _ :: rest -> go memory rest
    | Global { name; init; _ } :: rest ->
        let data = eval_expr ~ctx memory init in
        go ((name, data) :: memory) rest
  in
  go [] definitions

type evaled = { symbol : symbol; cell : memory_cell; ty : Syntax.Type.tvar }

let eval : ctx:Ctx.t -> program -> evaled list =
 fun ~ctx program ->
  let memory = eval_globals ~ctx program in
  List.filter_map
    (function
      | Global { name; entry_ty = Some ty; _ } ->
          Some { symbol = name; cell = List.assoc name memory; ty }
      | _ -> None)
    program
