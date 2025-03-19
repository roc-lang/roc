type symbol = [ `Sym of string ] [@@deriving show, eq]

let unsafe_from_string s = `Sym s
let show_symbol_raw (`Sym s) = s

type t = {
  fresh_symbol : string -> symbol;
  fresh_symbol_named : string -> symbol;
  idents : (symbol, string) Hashtbl.t;
  symenv : (string, symbol) Hashtbl.t;
}

let make () : t =
  let idents = Hashtbl.create ~random:false 128 in
  let symenv = Hashtbl.create ~random:false 128 in
  let fresh_name = Util.fresh_name_generator () in
  let fresh_symbol hint = `Sym (fresh_name hint) in
  let fresh_symbol_named hint =
    let sym = fresh_symbol hint in
    Hashtbl.add idents sym hint;
    sym
  in
  { fresh_symbol; fresh_symbol_named; idents; symenv }

let enter_scope { symenv; _ } name sym = Hashtbl.add symenv name sym
let exit_scope { symenv; _ } name = Hashtbl.remove symenv name

let scoped_name { symenv; _ } hint =
  match Hashtbl.find_opt symenv hint with
  | Some s -> s
  | None -> failwith (hint ^ " not found in scope")

let norm_of (`Sym s) = s

let syn_of { idents; _ } s =
  match Hashtbl.find_opt idents s with
  | Some s -> s
  | None ->
      let (`Sym s) = s in
      s

module SymbolMap = struct
  include Map.Make (struct
    type t = symbol

    let compare = compare
  end)

  let union ?(checker = None) ?(pp = fun _ -> "") u v =
    let f k x y =
      (match checker with
      | Some checker ->
          if not (checker k x y) then
            failwith
              ("SymbolMap.union: conflicting bindings on " ^ show_symbol_raw k
             ^ "\n" ^ pp x ^ " vs " ^ pp y)
      | None -> ());
      Some x
    in
    union f u v

  let union_uc ?(pp = fun _ -> "") u v = union ~checker:None ~pp u v

  let diff u v =
    let f _ x y = match (x, y) with Some x, None -> Some x | _ -> None in
    merge f u v

  let remove_keys (keys : symbol list) m =
    let f k _ = not (List.mem k keys) in
    filter f m

  let concat ?(checker = None) ?(pp = fun _ -> "") lst =
    List.fold_left (union ~checker ~pp) empty lst

  let concat_uc ?(pp = fun _ -> "") lst = concat ~checker:None ~pp lst
  let symmetric_diff u v = union (diff u v) (diff v u)
end

module SymbolSet = struct
  include Set.Make (struct
    type t = symbol

    let compare = compare
  end)

  let concat lst = List.fold_left union empty lst
end

let pp_symbol : t -> Format.formatter -> symbol -> unit =
 fun symbols f s -> Format.pp_print_string f (syn_of symbols s)
