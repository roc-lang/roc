open Ast
open Type
open Symbol

let rec pe_tvar : tvar -> unit =
 fun t ->
  match tvar_deref @@ unlink t with
  | Link _ -> failwith "pe_tvar: expected non-linked type"
  | Unbd -> ()
  | ForA -> ()
  | Content c -> (
      match c with
      | TFn (t1, tf, t2) ->
          pe_tvar t1;
          tvar_set tf ty_erased;
          pe_tvar t2
      | TTag tags -> List.iter (fun (_, ts) -> List.iter pe_tvar ts) tags
      | TRecord fields -> List.iter (fun (_, t) -> pe_tvar t) fields
      | TPrim _ -> ()
      | LSet lset ->
          SymbolMap.iter
            (fun _ captures -> SymbolMap.iter (fun _ t -> pe_tvar t) captures)
            lset)

let rec pe_expr : e_expr -> unit =
 fun (t, e) ->
  match e with
  | Var _ -> ()
  | Int _ -> ()
  | Str _ -> ()
  | Tag (_, es) -> List.iter pe_expr es
  | Record fields -> List.iter (fun (_, e) -> pe_expr e) fields
  | Access (e, _) -> pe_expr e
  | Let (_, e1, e2) ->
      pe_expr e1;
      pe_expr e2
  | Call (e1, e2) ->
      pe_expr e1;
      pe_expr e2
  | KCall (kfn, es) -> (
      List.iter pe_expr es;
      match kfn with
      | `Erase ->
          (* ~erase k : mark all functions reached by k as erased. *)
          pe_tvar (fst @@ List.hd es)
      | `Unerase ->
          (* let x = ~unerase _ : mark all functions reached by x as erased. *)
          pe_tvar t
      | _ -> ())
  | When (e, bs) ->
      pe_expr e;
      List.iter (fun (_, e) -> pe_expr e) bs

let pe_def_val : def_val -> unit = function
  | `Fn { body = e; _ } -> pe_expr e
  | `Val e -> pe_expr e
  | `Run (e, _) -> pe_expr e

let pe_def : def -> unit = function _, d -> pe_def_val d

(** Propagate known erasure to function types. *)
let propagate_erasure : program -> unit =
 fun program -> List.iter pe_def program
