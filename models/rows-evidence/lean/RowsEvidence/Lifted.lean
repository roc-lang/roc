/-
Lifted IR reader and an independent typing check.

The format is version 2 of the Lifted IR exporter,
`src/postcheck/monotype_lifted/ir_export.zig` (`ROC_LIFTED_IR_EXPORT=<dir>`,
Debug or `-Ddev-diagnostics` compilers, `--specialize=yes` only); `FORMAT.md`
specifies it. The typing rules are written from design.md and the Lifted AST;
RULES.md lists them.
-/
import Std.Data.HashMap
import Std.Data.HashSet

namespace Lifted

/-! ## Syntax -/

inductive TyK where
  | prim (n : String) | zst | hole | erased
  | list (e : Nat) | box (e : Nat)
  | tuple (xs : List Nat)
  | func (args : List Nat) (ret : Nat)
  | record (fs : List (String × Nat))
  | tags (ts : List (String × List Nat))
  /-- kind, definition key `module:decl`, type name, args, backing -/
  | named (kind defn name : String) (args : List Nat) (backing : Option Nat)
  deriving Repr, Inhabited

structure Local where
  ty : Nat
  cap : Option Nat
  deriving Repr, Inhabited

/-- `(local, type)` -/
abbrev TL := List (Nat × Nat)

structure Fn where
  idx : Nat
  args : TL
  caps : TL
  ret : Nat
  body : Option Nat   -- none = hosted
  deriving Repr, Inhabited

structure Branch where
  pat : Nat
  stmts : List Nat
  guard : Option Nat
  body : Nat
  deriving Repr, Inhabited

inductive EK where
  | leaf (tag : String)                       -- unit, literals, crash, ...
  | local_ (l : Nat)
  | uninitPayload (cond : Nat)
  | staticData (child : Option Nat)
  | wrap (tag : String) (child : Nat)         -- comptime_value typed_boundary row_widen nominal dbg expect expect_err literal_rejected
  | list (xs : List Nat) | tuple (xs : List Nat)
  | record (fs : List (String × Nat))
  | recordUpdate (base : Nat) (fs : List (String × Nat))
  | tag (name : String) (xs : List Nat)
  | let_ (pat value rest : Nat)
  | fnRef (fn : Nat) (caps : List (Nat × Nat))
  | callValue (callee : Nat) (args : List Nat)
  | callProc (fn : Option Nat) (demand : Bool) (args : List Nat) (caps : List (Nat × Nat))
  | lowLevel (op : String) (args : List Nat)
  | fieldAccess (recv : Nat) (names : List String)
  | tupleAccess (tup idx : Nat)
  | structEq (l r : Nat) (neg : Bool)
  | structHash (v h : Nat)
  | match_ (scrut : Nat) (bs : List Branch)
  | if_ (bs : List (Nat × Nat)) (els : Nat)
  | ifInitPayload (cond payload init uninit : Nat)
  | trySeq (tryE okLocal : Nat) (errTarget : Option Nat) (okBody : Nat)
  | tryRecSeq (tryE valLocal : Nat) (valName : String) (restLocal : Nat) (restName : String)
      (errTarget : Option Nat) (body : Nat)
  | block (stmts : List Nat) (final : Nat)
  | loop_ (params : TL) (inits : List Nat) (body : Nat)
  | break_ (v : Option Nat)
  | continue_ (vals : List Nat)
  | joinPoint (id : Nat) (params : TL) (body rem : Nat)
  | jump (target : Nat) (args : List Nat) (loopParams : TL) (loopVals : List Nat)
  | return_ (value target : Nat)
  deriving Repr, Inhabited

structure Expr where
  ty : Nat
  tag : String
  k : EK
  deriving Repr, Inhabited

inductive PK where
  | leaf (tag : String)
  | bind (l : Nat)
  | as_ (p l : Nat)
  | record (fs : List (String × Nat))
  | tuple (ps : List Nat)
  | list (ps : List Nat) (restIndex : Option Nat) (restPat : Option Nat)
  | tag (name : String) (ps : List Nat)
  | nominal (p : Nat)
  | str (caps : List (Option Nat))
  deriving Repr, Inhabited

structure Pat where
  ty : Nat
  tag : String
  k : PK
  deriving Repr, Inhabited

inductive Stmt where
  | uninit (p : Nat)
  | let_ (p v : Nat) (rec : Bool)
  | wrap (tag : String) (v : Nat)   -- expr expect dbg
  | return_ (v target : Nat)
  | leaf (tag : String)             -- crash checked_error
  deriving Repr, Inhabited

structure Program where
  stage : String
  types : Array TyK
  locals : Std.HashMap Nat Local
  fns : Array Fn
  exprs : Std.HashMap Nat Expr
  pats : Std.HashMap Nat Pat
  stmts : Std.HashMap Nat Stmt
  /-- the function each node was written under (the exporter writes a shared node once) -/
  exprOwner : Std.HashMap Nat Nat

/-! ## Reading -/

/-- Percent-decoding of `Writer.name`: `%` alone is the empty name. -/
def decodeName (s : String) : String := Id.run do
  if s == "%" then return ""
  let cs := s.toList.toArray
  let mut out : Array UInt8 := #[]
  let mut i := 0
  let hex (c : Char) : Nat :=
    if c.isDigit then c.toNat - '0'.toNat
    else if 'A' ≤ c ∧ c ≤ 'F' then c.toNat - 'A'.toNat + 10
    else c.toNat - 'a'.toNat + 10
  while i < cs.size do
    if cs[i]! == '%' ∧ i + 2 < cs.size + 0 + 1 ∧ i + 2 ≤ cs.size - 1 then
      out := out.push (UInt8.ofNat (hex cs[i+1]! * 16 + hex cs[i+2]!))
      i := i + 3
    else
      for b in (String.singleton cs[i]!).toUTF8.toList do out := out.push b
      i := i + 1
  return (String.fromUTF8? (ByteArray.mk out)).getD s

/-- A parser over the tokens of one line. -/
abbrev P := StateT (List String) (Except String)

def tok : P String := do
  match ← get with
  | t :: r => set r; pure t
  | [] => throw "unexpected end of line"

def nat : P Nat := do
  let t ← tok
  match t.toNat? with
  | some n => pure n
  | none => throw s!"expected a number, got `{t}`"

def optNat : P (Option Nat) := do
  let t ← tok
  if t == "-" then pure none else
  match t.toNat? with
  | some n => pure (some n)
  | none => throw s!"expected a number or `-`, got `{t}`"

def name : P String := do pure (decodeName (← tok))

def lit (s : String) : P Unit := do
  let t ← tok
  unless t == s do throw s!"expected `{s}`, got `{t}`"

def many (p : P α) : P (List α) := do
  let n ← nat
  let mut out := #[]
  for _ in [0:n] do out := out.push (← p)
  pure out.toList

def nats : P (List Nat) := many nat
def typedLocals : P TL := many (do let l ← nat; let t ← nat; pure (l, t))
def capOps : P (List (Nat × Nat)) := many (do let c ← nat; let v ← nat; pure (c, v))
def fieldsP : P (List (String × Nat)) := many (do let n ← name; let v ← nat; pure (n, v))

def done : P Unit := do
  match ← get with
  | [] => pure ()
  | r => throw s!"trailing tokens: {r}"

def runLine (p : P α) (toks : List String) : Except String α :=
  match (do let a ← p; done; pure a).run toks with
  | .ok (a, _) => .ok a
  | .error e => .error e

def tyP : P TyK := do
  match ← tok with
  | "prim" => pure (.prim (← tok))
  | "zst" => pure .zst
  | "hole" => pure .hole
  | "erased" => pure .erased
  | "list" => pure (.list (← nat))
  | "box" => pure (.box (← nat))
  | "tuple" => pure (.tuple (← nats))
  | "func" => do let a ← nats; let r ← nat; pure (.func a r)
  | "record" => pure (.record (← fieldsP))
  | "tags" => pure (.tags (← many (do let n ← name; let ps ← nats; pure (n, ps))))
  | "named" => do
      let kind ← tok; let defn ← tok; let nm ← name; let args ← nats; let b ← optNat
      pure (.named kind defn nm args b)
  | k => throw s!"unknown type kind `{k}`"

def leafExprs : List String :=
  ["unit", "unreachable", "uninitialized", "crash", "checked_error", "comptime_exhaustiveness_failed",
   "int_lit", "frac_f32_lit", "frac_f64_lit", "dec_lit", "str_lit", "bytes_lit",
   "lambda", "def_ref", "fn_def"]

def wrapExprs : List String :=
  ["comptime_value", "typed_boundary", "row_widen", "nominal", "dbg", "expect", "expect_err", "literal_rejected"]

def exprP (tag : String) : P EK := do
  if leafExprs.contains tag then return .leaf tag
  if wrapExprs.contains tag then return .wrap tag (← nat)
  match tag with
  | "local" => pure (.local_ (← nat))
  | "uninitialized_payload" => pure (.uninitPayload (← nat))
  | "static_data_candidate" => pure (.staticData (← optNat))
  | "list" => pure (.list (← nats))
  | "tuple" => pure (.tuple (← nats))
  | "record" => pure (.record (← fieldsP))
  | "record_update" => do let b ← nat; pure (.recordUpdate b (← fieldsP))
  | "tag" => do let n ← name; pure (.tag n (← nats))
  | "let_" => do let p ← nat; let v ← nat; let r ← nat; pure (.let_ p v r)
  | "fn_ref" => do let f ← nat; pure (.fnRef f (← capOps))
  | "call_value" => do let c ← nat; pure (.callValue c (← nats))
  | "call_proc" => do
      let f ← optNat; let d ← nat; let a ← nats; let c ← capOps
      pure (.callProc f (d == 1) a c)
  | "low_level" => do let op ← tok; pure (.lowLevel op (← nats))
  | "field_access" => do let r ← nat; pure (.fieldAccess r (← many name))
  | "tuple_access" => do let t ← nat; pure (.tupleAccess t (← nat))
  | "structural_eq" => do let l ← nat; let r ← nat; pure (.structEq l r ((← nat) == 1))
  | "structural_hash" => do let v ← nat; pure (.structHash v (← nat))
  | "match_" => do
      let s ← nat
      let bs ← many (do
        let p ← nat; let st ← nats; let g ← optNat; let b ← nat
        pure ({ pat := p, stmts := st, guard := g, body := b } : Branch))
      pure (.match_ s bs)
  | "if_" => do
      let bs ← many (do let c ← nat; let b ← nat; pure (c, b))
      pure (.if_ bs (← nat))
  | "if_initialized_payload" => do
      let c ← nat; let p ← nat; let i ← nat; let u ← nat; pure (.ifInitPayload c p i u)
  | "try_sequence" => do
      let t ← nat; let ok ← nat; let e ← optNat; let b ← nat; pure (.trySeq t ok e b)
  | "try_record_sequence" => do
      let t ← nat; let vl ← nat; let vn ← name; let rl ← nat; let rn ← name; let e ← optNat; let b ← nat
      pure (.tryRecSeq t vl vn rl rn e b)
  | "block" => do let st ← nats; pure (.block st (← nat))
  | "loop_" => do let ps ← typedLocals; let is ← nats; pure (.loop_ ps is (← nat))
  | "break_" => pure (.break_ (← optNat))
  | "continue_" => pure (.continue_ (← nats))
  | "join_point" => do
      let id ← nat; let ps ← typedLocals; let b ← nat; pure (.joinPoint id ps b (← nat))
  | "jump" => do
      let t ← nat; let a ← nats; let lp ← typedLocals; pure (.jump t a lp (← nats))
  | "return_" => do let v ← nat; pure (.return_ v (← nat))
  | t => throw s!"unknown expression `{t}`"

def patP (tag : String) : P PK := do
  match tag with
  | "wildcard" | "int_lit" | "dec_lit" | "frac_f32_lit" | "frac_f64_lit" | "str_lit" => pure (.leaf tag)
  | "bind" => pure (.bind (← nat))
  | "as" => do let p ← nat; pure (.as_ p (← nat))
  | "record" => pure (.record (← fieldsP))
  | "tuple" => pure (.tuple (← nats))
  | "list" => do let ps ← nats; let ri ← optNat; let rp ← optNat; pure (.list ps ri rp)
  | "tag" => do let n ← name; pure (.tag n (← nats))
  | "nominal" => pure (.nominal (← nat))
  | "str_pattern" => pure (.str (← many optNat))
  | t => throw s!"unknown pattern `{t}`"

def stmtP : P Stmt := do
  match ← tok with
  | "uninitialized" => pure (.uninit (← nat))
  | "let_" => do let p ← nat; let v ← nat; pure (.let_ p v ((← nat) == 1))
  | "expr" => pure (.wrap "expr" (← nat))
  | "expect" => pure (.wrap "expect" (← nat))
  | "dbg" => pure (.wrap "dbg" (← nat))
  | "return_" => do let v ← nat; pure (.return_ v (← nat))
  | "crash" => pure (.leaf "crash")
  | "checked_error" => pure (.leaf "checked_error")
  | t => throw s!"unknown statement `{t}`"

def parse (text : String) : Except String Program := do
  let lines := (text.splitOn "\n").filter (· ≠ "") |>.toArray
  let mut i := 0
  let line (j : Nat) : List String := ((lines[j]?.getD "").splitOn " ")
  let err (j : Nat) (m : String) : String := s!"line {j+1}: {m}: `{lines[j]?.getD ""}`"
  unless line 0 == ["roc-lifted-ir", "2"] do throw (err 0 "not a version-2 Lifted IR export")
  let stage ← match line 1 with
    | ["stage", s] => pure s
    | _ => throw (err 1 "expected `stage`")
  i := 2
  -- types
  let nTypes ← match line i with
    | ["types", n] => (n.toNat?).elim (throw (err i "bad count")) pure
    | _ => throw (err i "expected `types`")
  i := i + 1
  let mut types : Array TyK := #[]
  for k in [0:nTypes] do
    match line i with
    | "t" :: idx :: rest =>
      unless idx.toNat? == some k do throw (err i s!"type {k} out of order")
      match runLine tyP rest with
      | .ok t => types := types.push t
      | .error e => throw (err i e)
    | _ => throw (err i "expected `t`")
    i := i + 1
  -- locals
  let nLocals ← match line i with
    | ["locals", n] => (n.toNat?).elim (throw (err i "bad count")) pure
    | _ => throw (err i "expected `locals`")
  i := i + 1
  let mut locals : Std.HashMap Nat Local := {}
  for _ in [0:nLocals] do
    match runLine (do lit "l"; let id ← nat; let ty ← nat; let c ← optNat; pure (id, ty, c)) (line i) with
    | .ok (id, ty, c) =>
      if locals.contains id then throw (err i s!"local {id} twice")
      locals := locals.insert id { ty := ty, cap := c }
    | .error e => throw (err i e)
    i := i + 1
  -- functions and their reachable nodes
  let nFns ← match line i with
    | ["fns", n] => (n.toNat?).elim (throw (err i "bad count")) pure
    | _ => throw (err i "expected `fns`")
  i := i + 1
  let mut fns : Array Fn := #[]
  let mut exprs : Std.HashMap Nat Expr := {}
  let mut pats : Std.HashMap Nat Pat := {}
  let mut stmts : Std.HashMap Nat Stmt := {}
  let mut owner : Std.HashMap Nat Nat := {}
  let fnP : P Fn := do
    lit "f"
    let idx ← nat; let a ← typedLocals; let c ← typedLocals; let r ← nat
    let bt ← tok
    let body ← if bt == "hosted" then pure none else
      match bt.toNat? with
      | some n => pure (some n)
      | none => throw s!"bad body `{bt}`"
    pure { idx := idx, args := a, caps := c, ret := r, body := body }
  while i < lines.size ∧ line i != ["end"] do
    match line i with
    | "f" :: _ =>
      match runLine fnP (line i) with
      | .ok f =>
        unless f.idx == fns.size do throw (err i s!"function {f.idx} out of order")
        fns := fns.push f
      | .error e => throw (err i e)
    | "e" :: idx :: ty :: tag :: rest =>
      if fns.isEmpty then throw (err i "node before any function")
      match idx.toNat?, ty.toNat?, runLine (exprP tag) rest with
      | some x, some t, .ok k =>
        if exprs.contains x then throw (err i s!"expr {x} twice")
        exprs := exprs.insert x { ty := t, tag := tag, k := k }
        owner := owner.insert x (fns.size - 1)
      | _, _, .error e => throw (err i e)
      | _, _, _ => throw (err i "bad expr header")
    | "p" :: idx :: ty :: tag :: rest =>
      match idx.toNat?, ty.toNat?, runLine (patP tag) rest with
      | some x, some t, .ok k =>
        if pats.contains x then throw (err i s!"pat {x} twice")
        pats := pats.insert x { ty := t, tag := tag, k := k }
      | _, _, .error e => throw (err i e)
      | _, _, _ => throw (err i "bad pattern header")
    | "s" :: idx :: rest =>
      match idx.toNat?, runLine stmtP rest with
      | some x, .ok s =>
        if stmts.contains x then throw (err i s!"stmt {x} twice")
        stmts := stmts.insert x s
      | _, .error e => throw (err i e)
      | _, _ => throw (err i "bad statement header")
    | _ => throw (err i "unexpected line")
    i := i + 1
  unless line i == ["end"] do throw (err i "missing `end`")
  unless fns.size == nFns do throw s!"header says {nFns} functions, found {fns.size}"
  unless i + 1 == lines.size do throw (err (i+1) "content after `end`")
  pure { stage := stage, types := types, locals := locals, fns := fns,
         exprs := exprs, pats := pats, stmts := stmts, exprOwner := owner }

/-! ## Types -/

namespace Program
variable (pg : Program)

def ty (t : Nat) : TyK := pg.types[t]!

/-- The structural type under every named backing. -/
def shape (t : Nat) : Nat := Id.run do
  let mut cur := t
  for _ in [0:pg.types.size] do
    match pg.ty cur with
    | .named _ _ _ _ (some b) => cur := b
    | _ => return cur
  return cur

/-- Transparent aliases compare through their backings (Monotype `typeViewEql`,
type.zig `typeViewEql`); nominals do not. -/
def unalias (t : Nat) : Nat := Id.run do
  let mut cur := t
  for _ in [0:pg.types.size] do
    match pg.ty cur with
    | .named "alias" _ _ _ (some b) => cur := b
    | _ => return cur
  return cur

/-- Structural type equality, by text names, coinductive over the type graph. -/
partial def eqT (a b : Nat) : Bool :=
  (go [(a, b)] {}).isSome
where
  go : List (Nat × Nat) → Std.HashSet (Nat × Nat) → Option Unit
  | [], _ => some ()
  | (x0, y0) :: rest, seen =>
    let x := pg.unalias x0
    let y := pg.unalias y0
    if x == y ∨ seen.contains (x, y) then go rest seen else
    let seen := seen.insert (x, y)
    let zipped (xs ys : List Nat) : Option (List (Nat × Nat)) :=
      if xs.length == ys.length then some (xs.zip ys) else none
    let more : Option (List (Nat × Nat)) :=
      match pg.ty x, pg.ty y with
      | .prim p, .prim q => if p == q then some [] else none
      | .zst, .zst | .hole, .hole | .erased, .erased => some []
      | .list e, .list f | .box e, .box f => some [(e, f)]
      | .tuple xs, .tuple ys => zipped xs ys
      | .func xa xr, .func ya yr => (zipped xa ya).map ((xr, yr) :: ·)
      | .record xs, .record ys =>
        if xs.map (·.1) == ys.map (·.1) then some ((xs.map (·.2)).zip (ys.map (·.2))) else none
      | .tags xs, .tags ys =>
        if xs.map (fun p => (p.1, p.2.length)) == ys.map (fun p => (p.1, p.2.length))
        then some ((xs.zip ys).flatMap (fun (p, q) => p.2.zip q.2)) else none
      | .named k d n xa xb, .named k' d' n' ya yb =>
        if k == k' ∧ d == d' ∧ n == n' then
          match zipped xa ya, xb, yb with
          | some ps, some b, some c => some ((b, c) :: ps)
          | some ps, none, none => some ps
          | _, _, _ => none
        else none
      | _, _ => none
    match more with
    | some ps => go (ps ++ rest) seen
    | none => none

/-- design.md "Row Joins": `v` has `t`'s structure, and every tag row of `v`,
at any depth, is a subset of the corresponding row of `t`. -/
partial def rowSub (v t : Nat) : Bool :=
  (go [(v, t)] {}).isSome
where
  go : List (Nat × Nat) → Std.HashSet (Nat × Nat) → Option Unit
  | [], _ => some ()
  | (x0, y0) :: rest, seen =>
    let x := pg.unalias x0
    let y := pg.unalias y0
    if x == y ∨ seen.contains (x, y) ∨ eqT pg x y then go rest seen else
    let seen := seen.insert (x, y)
    let zipped (xs ys : List Nat) : Option (List (Nat × Nat)) :=
      if xs.length == ys.length then some (xs.zip ys) else none
    let more : Option (List (Nat × Nat)) :=
      match pg.ty x, pg.ty y with
      | .list e, .list f | .box e, .box f => some [(e, f)]
      | .tuple xs, .tuple ys => zipped xs ys
      | .record xs, .record ys =>
        if xs.map (·.1) == ys.map (·.1) then some ((xs.map (·.2)).zip (ys.map (·.2))) else none
      | .tags xs, .tags ys =>
        xs.foldlM (fun acc (n, ps) =>
          match ys.find? (·.1 == n) with
          | some (_, qs) => if ps.length == qs.length then some (acc ++ ps.zip qs) else none
          | none => none) []
      | .named k d n xa xb, .named k' d' n' ya yb =>
        if k == k' ∧ d == d' ∧ n == n' then
          match zipped xa ya, xb, yb with
          | some ps, some b, some c => some ((b, c) :: ps)
          | some ps, none, none => some ps
          | _, _, _ => none
        else none
      | _, _ => none
    match more with
    | some ps => go (ps ++ rest) seen
    | none => none

/-- design.md "Shaped requests" (22355-22394): a demand-shaped callee's
parameter or result `p` stands for the call's type `a` when it is `a` itself,
a hole (as a `List` item or `Box` payload), or `a`'s layout representative: a
record becomes the tuple of its fields in row order, a nominal its backing's
representative, and lists, boxes and tuples keep their structure with
representative components. -/
partial def shapedAccepts (a p : Nat) : Bool :=
  if eqT pg a p then true else
  match pg.ty a, pg.ty p with
  | _, .hole => true
  | .named _ _ _ _ (some b), _ => shapedAccepts b p
  | .record fs, .tuple ts =>
    fs.length == ts.length && (fs.zip ts).all fun ((_, f), t) => shapedAccepts f t
  | .tuple xs, .tuple ys => xs.length == ys.length && (xs.zip ys).all fun (x, y) => shapedAccepts x y
  | .list x, .list y | .box x, .box y => shapedAccepts x y
  -- the template's own structure stays; only the slots its substitution fills are shaped
  | .record fs, .record gs =>
    fs.map (·.1) == gs.map (·.1) && (fs.zip gs).all fun ((_, f), (_, g)) => shapedAccepts f g
  | .tags xs, .tags ys =>
    xs.map (fun p => (p.1, p.2.length)) == ys.map (fun p => (p.1, p.2.length)) &&
    (xs.zip ys).all fun ((_, ps), (_, qs)) => (ps.zip qs).all fun (x, y) => shapedAccepts x y
  | .named k d n xa _, .named k' d' n' ya _ =>
    k == k' && d == d' && n == n' && xa.length == ya.length && (xa.zip ya).all fun (x, y) => shapedAccepts x y
  | _, _ => false

partial def showT (t : Nat) (depth : Nat := 4) : String :=
  if depth == 0 then s!"#{t}" else
  let r := fun u => showT u (depth - 1)
  let commas (xs : List String) := ", ".intercalate xs
  match pg.types[t]? with
  | none => s!"#{t}?"
  | some (.prim p) => p
  | some .zst => "zst" | some .hole => "hole" | some .erased => "erased"
  | some (.list e) => s!"List({r e})"
  | some (.box e) => s!"Box({r e})"
  | some (.tuple xs) => s!"({commas (xs.map r)})"
  | some (.func a b) => s!"({commas (a.map r)} -> {r b})"
  | some (.record fs) => "{ " ++ commas (fs.map fun (n, u) => s!"{n} : {r u}") ++ " }"
  | some (.tags ts) =>
    "[" ++ commas (ts.map fun (n, ps) => if ps.isEmpty then n else s!"{n}({commas (ps.map r)})") ++ "]"
  | some (.named _ _ n a _) => if a.isEmpty then n else s!"{n}({commas (a.map r)})"

end Program

/-! ## Checking -/

structure Viol where
  rule : String
  fn : Nat
  site : String
  msg : String
  deriving Repr

structure St where
  out : Array Viol := #[]
  fn : Nat := 0
  bound : Std.HashSet Nat := {}
  used : Array (Nat × String) := #[]
  visitedE : Std.HashSet Nat := {}

abbrev C := ReaderT Program (StateM St)

def report (rule site msg : String) : C Unit :=
  modify fun s => { s with out := s.out.push { rule := rule, fn := s.fn, site := site, msg := msg } }

def transfers : List String :=
  ["unreachable", "return_", "break_", "continue_", "jump", "crash", "checked_error",
   "expect_err", "literal_rejected", "comptime_exhaustiveness_failed"]

/-- The expression never produces a value (Lambda Solved: terminal expressions contribute no flow). -/
def diverges (pg : Program) (e : Nat) : Bool :=
  match pg.exprs[e]? with
  | none => false
  | some x =>
    transfers.contains x.tag ||
    (match x.k with
     | .block _ f => (pg.exprs[f]?.map (·.tag)) == some "unreachable"
     | _ => false)

def eTy (pg : Program) (e : Nat) : Option Nat := pg.exprs[e]?.map (·.ty)
def pTy (pg : Program) (p : Nat) : Option Nat := pg.pats[p]?.map (·.ty)

def expectT (rule site what : String) (actual expected : Nat) : C Unit := do
  let pg ← read
  unless pg.eqT actual expected do
    report rule site s!"{what}: {pg.showT actual} is not {pg.showT expected}"

/-- The value of `child` has type `expected` exactly, unless it never produces a value. -/
def expectE (rule site what : String) (child expected : Nat) : C Unit := do
  let pg ← read
  if diverges pg child then return
  match eTy pg child with
  | some t => expectT rule site s!"{what} (e {child})" t expected
  | none => pure ()

def expectP (rule site what : String) (p expected : Nat) : C Unit := do
  match pTy (← read) p with
  | some t => expectT rule site s!"{what} (p {p})" t expected
  | none => pure ()

def bind (l : Nat) : C Unit := modify fun s => { s with bound := s.bound.insert l }
def bindTL (ls : TL) : C Unit := do
  let pg ← read
  for (l, t) in ls do
    bind l
    match pg.locals[l]? with
    | some loc => expectT "L-TypedLocal" s!"local {l}" "typed local" t loc.ty
    | none => report "WF-Local" s!"local {l}" "undeclared local"

def tagPayloads (pg : Program) (t : Nat) (n : String) : Option (List Nat) :=
  match pg.ty (pg.shape t) with
  | .tags ts => (ts.find? (·.1 == n)).map (·.2)
  | _ => none

/-- The capture slot types of function `f`, by capture id. -/
def capSlots (pg : Program) (f : Fn) : List (Nat × Nat) :=
  f.caps.filterMap fun (l, t) => (pg.locals[l]?.bind (·.cap)).map (·, t)

/-- `Bool`: the builtin nominal whose backing is `[False, True]`. -/
def isBool (pg : Program) (t : Nat) : Bool :=
  match pg.ty (pg.shape t) with
  | .tags ts => ts.map (fun p => (p.1, p.2.length)) == [("False", 0), ("True", 0)]
  | .prim "bool" => true
  | _ => false

def numericPrims : List String :=
  ["u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "u128", "i128", "f32", "f64", "dec"]

def litPrims : String → Option (List String)
  | "int_lit" => some numericPrims
  | "dec_lit" => some ["dec"]
  | "frac_f32_lit" => some ["f32"]
  | "frac_f64_lit" => some ["f64"]
  | "str_lit" => some ["str"]
  | _ => none

def expectBool (rule site what : String) (e : Nat) : C Unit := do
  let pg ← read
  if diverges pg e then return
  if let some t := eTy pg e then
    unless isBool pg t do report rule site s!"{what} (e {e}) is {pg.showT t}, not Bool"

def checkCaps (rule site : String) (f : Fn) (ops : List (Nat × Nat)) : C Unit := do
  let pg ← read
  let slots := capSlots pg f
  unless ops.length == slots.length do
    report "T-Capture" site s!"{ops.length} capture operands for {slots.length} slots of f{f.idx}"
  for (c, v) in ops do
    match slots.find? (·.1 == c) with
    | some (_, t) => expectE "T-Capture" site s!"capture {c}" v t
    | none => report "T-Capture" site s!"capture {c} is not a slot of f{f.idx}"
  let _ := rule

mutual
partial def checkPat (p : Nat) : C Unit := do
  let pg ← read
  let some x := pg.pats[p]? | report "WF-Node" s!"p {p}" "pattern not in the export"
  let site := s!"p {p}"
  match x.k with
  | .leaf tag =>
    if let some ok := litPrims tag then
      let fine := match pg.ty (pg.shape x.ty) with
        | .prim p => ok.contains p
        | _ => false
      unless fine do report "P-Literal" site s!"{tag} pattern at {pg.showT x.ty}"
  | .bind l =>
    bind l
    match pg.locals[l]? with
    | some loc => expectT "P-Bind" site "bound local" loc.ty x.ty
    | none => report "WF-Local" site s!"undeclared local {l}"
  | .as_ q l =>
    bind l
    checkPat q
    expectP "P-As" site "inner pattern" q x.ty
    if let some loc := pg.locals[l]? then expectT "P-As" site "bound local" loc.ty x.ty
  | .record fs =>
    match pg.ty (pg.shape x.ty) with
    | .record tfs =>
      for (n, q) in fs do
        checkPat q
        match tfs.find? (·.1 == n) with
        | some (_, t) => expectP "P-Record" site s!"field {n}" q t
        | none => report "P-Record" site s!"field {n} not in {pg.showT x.ty}"
    | _ => report "P-Record" site s!"record pattern at {pg.showT x.ty}"
  | .tuple qs =>
    match pg.ty (pg.shape x.ty) with
    | .tuple ts =>
      if qs.length != ts.length then report "P-Tuple" site "arity" else
      for (q, t) in qs.zip ts do checkPat q; expectP "P-Tuple" site "element" q t
    | _ => report "P-Tuple" site s!"tuple pattern at {pg.showT x.ty}"
  | .list qs _ rp =>
    match pg.ty (pg.shape x.ty) with
    | .list el =>
      for q in qs do checkPat q; expectP "P-List" site "element" q el
      if let some r := rp then checkPat r; expectP "P-List" site "rest" r x.ty
    | _ => report "P-List" site s!"list pattern at {pg.showT x.ty}"
  | .tag n qs =>
    match tagPayloads pg x.ty n with
    | some ts =>
      if qs.length != ts.length then report "P-Tag" site s!"{n} has {ts.length} payloads, pattern has {qs.length}" else
      for (q, t) in qs.zip ts do checkPat q; expectP "P-Tag" site s!"{n} payload" q t
    | none => report "P-Tag" site s!"tag {n} not in {pg.showT x.ty}"
    for q in qs do checkPat q
  | .nominal q =>
    checkPat q
    match pg.ty x.ty with
    | .named _ _ _ _ (some b) => expectP "P-Nominal" site "backing pattern" q b
    | _ => expectP "P-Nominal" site "transparent nominal pattern" q x.ty
  | .str caps => for c in caps do if let some q := c then checkPat q

partial def checkStmt (s : Nat) (ret : Nat) : C Unit := do
  let pg ← read
  let some x := pg.stmts[s]? | report "WF-Node" s!"s {s}" "statement not in the export"
  let site := s!"s {s}"
  match x with
  | .uninit p => checkPat p
  | .let_ p v _ =>
    checkPat p
    checkExpr v ret
    if let some t := pTy pg p then expectE "S-Let" site "bound value" v t
  | .wrap _ v => checkExpr v ret
  | .return_ v target => checkReturn site v target ret
  | .leaf _ => pure ()

partial def checkReturn (site : String) (v target ret : Nat) : C Unit := do
  let pg ← read
  checkExpr v ret
  unless pg.eqT target ret do
    report "T-Return" site s!"target {pg.showT target} is not the function's result {pg.showT ret}"
  if !diverges pg v then
    if let some t := eTy pg v then
      unless pg.rowSub t target do
        report "T-Return" site s!"returned {pg.showT t} does not row-join into {pg.showT target}"

partial def checkExpr (e : Nat) (ret : Nat) : C Unit := do
  let pg ← read
  if (← get).visitedE.contains e then return
  modify fun s => { s with visitedE := s.visitedE.insert e }
  let some x := pg.exprs[e]? | report "WF-Node" s!"e {e}" "expression not in the export"
  let site := s!"e {e} {x.tag}"
  let sh := pg.ty (pg.shape x.ty)
  match x.k with
  | .leaf tag =>
    if let some ok := litPrims tag then
      -- Monotype writes `True`/`False` as an integer literal at `Bool`
      -- (lower.zig `BodyContext.boolLiteral`)
      let fine := match sh with
        | .prim p => ok.contains p || (tag == "int_lit" && p == "bool")
        | _ => tag == "int_lit" && isBool pg x.ty
      unless fine do report "T-Literal" site s!"{tag} at {pg.showT x.ty}"
  | .local_ l =>
    modify fun s => { s with used := s.used.push (l, site) }
    match pg.locals[l]? with
    | some loc => expectT "T-Local" site "local" x.ty loc.ty
    | none => report "WF-Local" site s!"undeclared local {l}"
  | .uninitPayload _ => pure ()
  | .staticData c => if let some v := c then checkExpr v ret; expectE "T-StaticData" site "value" v x.ty
  | .wrap tag v =>
    checkExpr v ret
    match tag with
    | "row_widen" =>
      if !diverges pg v then
        if let some t := eTy pg v then
          unless pg.rowSub t x.ty do report "T-RowWiden" site s!"{pg.showT t} does not widen to {pg.showT x.ty}"
    | "nominal" =>
      match pg.ty x.ty with
      | .named _ _ _ _ (some b) => expectE "T-Nominal" site "backing value" v b
      | _ => expectE "T-Nominal" site "transparent nominal value" v x.ty
    | "typed_boundary" | "comptime_value" => expectE "T-TypedBoundary" site "value" v x.ty
    | _ => pure ()
  | .list xs =>
    for v in xs do checkExpr v ret
    match sh with
    | .list el => for v in xs do expectE "T-List" site "element" v el
    | _ => report "T-List" site s!"list at {pg.showT x.ty}"
  | .tuple xs =>
    for v in xs do checkExpr v ret
    match sh with
    | .tuple ts =>
      if xs.length != ts.length then report "T-Tuple" site "arity" else
      for (v, t) in xs.zip ts do expectE "T-Tuple" site "element" v t
    | _ => report "T-Tuple" site s!"tuple at {pg.showT x.ty}"
  | .record fs =>
    for (_, v) in fs do checkExpr v ret
    match sh with
    | .record tfs =>
      -- a record expression lists its fields in source order; the type's order is canonical
      unless fs.length == tfs.length && fs.all (fun (n, _) => tfs.any (·.1 == n)) do
        report "T-Record" site s!"fields {fs.map (·.1)} vs type {tfs.map (·.1)}"
      for (n, v) in fs do
        if let some (_, t) := tfs.find? (·.1 == n) then expectE "T-Record" site s!"field {n}" v t
    | _ => report "T-Record" site s!"record at {pg.showT x.ty}"
  | .recordUpdate b fs =>
    checkExpr b ret
    expectE "T-RecordUpdate" site "base" b x.ty
    for (_, v) in fs do checkExpr v ret
    match sh with
    | .record tfs =>
      for (n, v) in fs do
        match tfs.find? (·.1 == n) with
        | some (_, t) => expectE "T-RecordUpdate" site s!"field {n}" v t
        | none => report "T-RecordUpdate" site s!"field {n} not in {pg.showT x.ty}"
    | _ => report "T-RecordUpdate" site s!"update at {pg.showT x.ty}"
  | .tag n xs =>
    for v in xs do checkExpr v ret
    match tagPayloads pg x.ty n with
    | some ts =>
      if xs.length != ts.length then report "T-Tag" site s!"{n} has {ts.length} payloads, given {xs.length}" else
      for (v, t) in xs.zip ts do expectE "T-Tag" site s!"{n} payload" v t
    | none => report "T-Tag" site s!"tag {n} not in {pg.showT x.ty}"
  | .let_ p v r =>
    checkPat p; checkExpr v ret; checkExpr r ret
    if let some t := pTy pg p then expectE "T-Let" site "bound value" v t
    expectE "T-Let" site "rest" r x.ty
  | .fnRef f caps =>
    for (_, v) in caps do checkExpr v ret
    match pg.fns[f]? with
    | none => report "WF-Fn" site s!"function {f} out of range"
    | some fd =>
      checkCaps "T-FnRef" site fd caps
      match sh with
      | .func as r =>
        if as.length != fd.args.length then report "T-FnRef" site "arity" else
        for (a, (_, t)) in as.zip fd.args do expectT "T-FnRef" site "parameter" a t
        expectT "T-FnRef" site "result" r fd.ret
      | _ => report "T-FnRef" site s!"function reference at {pg.showT x.ty}"
  | .callValue c as =>
    checkExpr c ret
    for v in as do checkExpr v ret
    match eTy pg c with
    | some ct =>
      match pg.ty (pg.shape ct) with
      | .func ps r =>
        if as.length != ps.length then report "T-CallValue" site "arity" else
        for (v, t) in as.zip ps do expectE "T-CallValue" site "argument" v t
        expectT "T-CallValue" site "result" x.ty r
      | _ => report "T-CallValue" site s!"callee at {pg.showT ct}"
    | none => pure ()
  | .callProc f demand as caps =>
    for v in as do checkExpr v ret
    for (_, v) in caps do checkExpr v ret
    match f with
    | none => pure ()
    | some fi =>
      match pg.fns[fi]? with
      | none => report "WF-Fn" site s!"function {fi} out of range"
      | some fd =>
        checkCaps "T-CallProc" site fd caps
        if as.length != fd.args.length then report "T-CallProc" site s!"{as.length} arguments to f{fi} of arity {fd.args.length}" else
        if demand then
          -- T-CallShaped: the call keeps its own types; the callee holds holes and representatives
          for (v, (_, t)) in as.zip fd.args do
            if let some aty := eTy pg v then
              unless diverges pg v || pg.shapedAccepts aty t do
                report "T-CallShaped" site s!"argument (e {v}) {pg.showT aty} is not shaped as f{fi}'s parameter {pg.showT t}"
          unless pg.shapedAccepts x.ty fd.ret do
            report "T-CallShaped" site s!"result {pg.showT x.ty} is not shaped as f{fi}'s result {pg.showT fd.ret}"
        else
          for (v, (_, t)) in as.zip fd.args do expectE "T-CallProc" site "argument" v t
          expectT "T-CallProc" site s!"result of f{fi}" x.ty fd.ret
  | .lowLevel _ as => for v in as do checkExpr v ret
  | .fieldAccess r ns =>
    checkExpr r ret
    match eTy pg r with
    | none => pure ()
    | some rt =>
      let rec walk (t : Nat) : List String → Option Nat
        | [] => some t
        | n :: rest =>
          match pg.ty (pg.shape t) with
          | .record fs => (fs.find? (·.1 == n)).bind fun (_, u) => walk u rest
          | _ => none
      match walk rt ns with
      | some t => expectT "T-FieldAccess" site s!"field {ns}" x.ty t
      | none => report "T-FieldAccess" site s!"no field {ns} in {pg.showT rt}"
  | .tupleAccess t i =>
    checkExpr t ret
    if let some tt := eTy pg t then
      match pg.ty (pg.shape tt) with
      | .tuple ts =>
        match ts[i]? with
        | some u => expectT "T-TupleAccess" site "element" x.ty u
        | none => report "T-TupleAccess" site "index out of range"
      | _ => report "T-TupleAccess" site s!"tuple access on {pg.showT tt}"
  | .structEq l r _ =>
    checkExpr l ret; checkExpr r ret
    if let (some a, some b) := (eTy pg l, eTy pg r) then
      unless diverges pg l || diverges pg r do expectT "T-StructuralEq" site "operands" a b
    unless isBool pg x.ty do report "T-StructuralEq" site s!"result {pg.showT x.ty} is not Bool"
  | .structHash v h => checkExpr v ret; checkExpr h ret
  | .match_ s bs =>
    checkExpr s ret
    for b in bs do
      checkPat b.pat
      for st in b.stmts do checkStmt st ret
      if let some g := b.guard then checkExpr g ret; expectBool "T-Cond" site "guard" g
      checkExpr b.body ret
      if let some st := eTy pg s then expectP "T-Match" site "branch pattern" b.pat st
      expectE "T-Match" site "branch body" b.body x.ty
  | .if_ bs els =>
    for (c, b) in bs do
      checkExpr c ret; checkExpr b ret
      expectBool "T-Cond" site "condition" c
      expectE "T-If" site "branch" b x.ty
    checkExpr els ret
    expectE "T-If" site "else" els x.ty
  | .ifInitPayload c p i u =>
    bind p
    checkExpr c ret; checkExpr i ret; checkExpr u ret
    expectE "T-InitializedPayload" site "initialized" i x.ty
    expectE "T-InitializedPayload" site "uninitialized" u x.ty
  | .trySeq t ok _ b =>
    bind ok
    checkExpr t ret; checkExpr b ret
    expectE "T-Try" site "ok body" b x.ty
  | .tryRecSeq t vl _ rl _ _ b =>
    bind vl; bind rl
    checkExpr t ret; checkExpr b ret
    expectE "T-Try" site "ok body" b x.ty
  | .block ss f =>
    for s in ss do checkStmt s ret
    checkExpr f ret
    expectE "T-Block" site "final expression" f x.ty
  | .loop_ ps is b =>
    bindTL ps
    for v in is do checkExpr v ret
    if is.length != ps.length then report "T-Loop" site "initial values vs parameters" else
    for (v, (_, t)) in is.zip ps do expectE "T-Loop" site "initial value" v t
    checkExpr b ret
  | .break_ v => if let some w := v then checkExpr w ret
  | .continue_ vs => for v in vs do checkExpr v ret
  | .joinPoint _ ps b r =>
    bindTL ps
    checkExpr b ret; checkExpr r ret
    expectE "T-Join" site "body" b x.ty
    expectE "T-Join" site "remainder" r x.ty
  | .jump _ as lp lv =>
    for v in as do checkExpr v ret
    for v in lv do checkExpr v ret
    if lv.length != lp.length then report "T-JumpLoop" site "loop values vs parameters" else
    for (v, (_, t)) in lv.zip lp do expectE "T-JumpLoop" site "loop value" v t
  | .return_ v target => checkReturn site v target ret
end

/-- C-Closed: a demand hole occurs only as a `List` or `Box` element. -/
partial def holeOutsideElem (pg : Program) (t : Nat) : Bool :=
  go [t] {}
where
  go : List Nat → Std.HashSet Nat → Bool
  | [], _ => false
  | u :: rest, seen =>
    if seen.contains u then go rest seen else
    let seen := seen.insert u
    let elem (e : Nat) : List Nat := if pg.ty e matches .hole then [] else [e]
    match pg.ty u with
    | .hole => true
    | .list e | .box e => go (elem e ++ rest) seen
    | .tuple xs => go (xs ++ rest) seen
    | .func a r => go (r :: a ++ rest) seen
    | .record fs => go (fs.map (·.2) ++ rest) seen
    | .tags ts => go (ts.flatMap (·.2) ++ rest) seen
    | .named _ _ _ a b => go (a ++ b.toList ++ rest) seen
    | _ => go rest seen

def checkWF : C Unit := do
  let pg ← read
  let n := pg.types.size
  let inRange (t : Nat) := t < n
  for i in [0:n] do
    let refs : List Nat := match pg.types[i]! with
      | .list e | .box e => [e]
      | .tuple xs => xs
      | .func a r => r :: a
      | .record fs => fs.map (·.2)
      | .tags ts => ts.flatMap (·.2)
      | .named _ _ _ a b => a ++ b.toList
      | _ => []
    for r in refs do unless inRange r do report "WF-Type" s!"t {i}" s!"type {r} out of range"
  for (l, loc) in pg.locals.toList do
    unless inRange loc.ty do report "WF-Type" s!"l {l}" "type out of range"
  for (e, x) in pg.exprs.toList do
    unless inRange x.ty do report "WF-Type" s!"e {e}" "type out of range"
  for (p, x) in pg.pats.toList do
    unless inRange x.ty do report "WF-Type" s!"p {p}" "type out of range"

def checkProgram (pg : Program) : Array Viol := Id.run do
  let (_, st0) := (checkWF.run pg).run {}
  if !st0.out.isEmpty then return st0.out
  let mut out := st0.out
  -- C-Closed over every typed position
  for (l, loc) in pg.locals.toList do
    if holeOutsideElem pg loc.ty then out := out.push { rule := "C-Closed", fn := 0, site := s!"l {l}", msg := pg.showT loc.ty }
  for (e, x) in pg.exprs.toList do
    if holeOutsideElem pg x.ty then out := out.push { rule := "C-Closed", fn := pg.exprOwner[e]?.getD 0, site := s!"e {e} {x.tag}", msg := pg.showT x.ty }
  for (p, x) in pg.pats.toList do
    if holeOutsideElem pg x.ty then out := out.push { rule := "C-Closed", fn := 0, site := s!"p {p} {x.tag}", msg := pg.showT x.ty }
  for f in pg.fns do
    let some body := f.body | continue
    let act : C Unit := do
      bindTL f.args
      bindTL f.caps
      for (l, _) in f.caps do
        if (pg.locals[l]?.bind (·.cap)).isNone then report "T-Capture" s!"f {f.idx}" s!"capture local {l} has no capture id"
      checkExpr body f.ret
      if !diverges pg body then
        if let some t := eTy pg body then
          unless pg.rowSub t f.ret do
            report "F-Body" s!"f {f.idx}" s!"body {pg.showT t} does not row-join into result {pg.showT f.ret}"
      -- C-Scope: every local a body reads is bound in that body
      let st ← get
      for (l, site) in st.used do
        unless st.bound.contains l do report "C-Scope" site s!"local {l} is not bound in f{f.idx}"
    let (_, st) := (act.run pg).run { fn := f.idx }
    out := out ++ st.out
  return out

def summary (pg : Program) : String :=
  let shared := pg.exprs.size - pg.exprOwner.size
  s!"stage {pg.stage}: {pg.types.size} types, {pg.locals.size} locals, {pg.fns.size} fns, {pg.exprs.size} exprs, {pg.pats.size} pats, {pg.stmts.size} stmts{if shared > 0 then s!", {shared} unowned" else ""}"

end Lifted
