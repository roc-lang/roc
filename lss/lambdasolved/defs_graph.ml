open Ast
open Symbol

module G = Graph.Imperative.Digraph.Concrete (struct
  type t = symbol

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end)

module Components = Graph.Components.Make (G)

let build_definition_map : def list -> def SymbolMap.t =
 fun defs ->
  let map_def = function ((_, x), _) as def -> (x, def) in
  SymbolMap.of_seq @@ List.to_seq (List.map map_def defs)

let edges_sym def_map s =
  if SymbolMap.mem s def_map then SymbolSet.singleton s else SymbolSet.empty

let edges_expr def_map e =
  let rec go (_, e) =
    match e with
    | Var v -> edges_sym def_map v
    | Int _ -> SymbolSet.empty
    | Str _ -> SymbolSet.empty
    | Tag (_, es) -> SymbolSet.concat (List.map go es)
    | Record fields -> SymbolSet.concat (List.map go @@ List.map snd fields)
    | Access (e, _) -> go e
    | Let (_, e, r) -> SymbolSet.union (go e) (go r)
    | Call (e1, e2) -> SymbolSet.union (go e1) (go e2)
    | KCall (_, es) -> SymbolSet.concat (List.map go es)
    | When (e, bs) ->
        SymbolSet.union (go e) (SymbolSet.concat (List.map go_branch bs))
  and go_branch (p, e) = SymbolSet.union (go_pat p) (go e)
  and go_pat (_, p) =
    match p with
    | PTag (_, es) -> SymbolSet.concat (List.map go_pat es)
    | PVar _ -> SymbolSet.empty
  in
  go e

let edges_def def_map (((_, x), dv) : def) =
  let edges =
    match dv with
    | `Fn { arg = _; captures = _; body } -> edges_expr def_map body
    | `Val e -> edges_expr def_map e
    | `Run (e, _) -> edges_expr def_map e
  in
  (x, edges)

let scc_defs : def list -> def list list =
 fun defs ->
  let graph = G.create () in
  let add_node = G.add_vertex graph in
  let def_map = build_definition_map defs in
  SymbolMap.iter (fun name _ -> add_node name) def_map;
  List.iter
    (fun def ->
      let x, edges = edges_def def_map def in
      SymbolSet.iter (fun dep -> G.add_edge graph x dep) edges)
    defs;
  let scc_def_names = Components.scc_list graph in
  let scc_defs =
    List.map
      (fun names -> List.map (fun name -> SymbolMap.find name def_map) names)
      scc_def_names
  in
  scc_defs
