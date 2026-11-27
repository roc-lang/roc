/-
  Type store and row unification, modeled on the checker's union-find store.

  Rule names (R-*) are the ones used in RULES.md, which cites design.md and the Zig source for each.
-/
namespace RE

/-- A where-requirement carried by a flex/rigid variable: method name and signature var.
    (types.zig `StaticDispatchConstraint`) -/
structure Req where
  meth : String
  sig  : Nat
  deriving Repr, BEq, Inhabited

/-- Variable descriptor flags that the rules read (the `Descriptor` flags in types.zig). -/
structure Marks where
  /-- annotation-minted open tail: may close, may never gain a tag (R-Bounded). -/
  bounded     : Bool := false
  /-- `[]` written by inference (exhaustive match / closed pattern): closed, but liftable
      into a nominal and generalized like a variable (R-MatchClose, M-Copy). -/
  uncommitted : Bool := false
  reqs        : List Req := []
  deriving Repr, BEq, Inhabited

/-- Flat type: children are variable ids. -/
inductive FT where
  | str | u64
  | fn (args : List Nat) (ret : Nat)
  | record (fs : List (String × Nat)) (ext : Nat)
  | emptyRec
  | tags (ts : List (String × List Nat)) (ext : Nat)
  | emptyTags
  | nom (name : String) (args : List Nat)
  | list (e : Nat)
  deriving Repr, BEq, Inhabited

inductive Content where
  | link (n : Nat)
  | flex (m : Marks)
  | rigid (name : String) (m : Marks)
  | struct (ft : FT) (m : Marks)
  deriving Repr, BEq, Inhabited

/-- Generalized variables carry this level. -/
def genericLevel : Nat := 1000000

structure Slot where
  c     : Content
  level : Nat
  deriving Repr, Inhabited

/-- Why a unification failed. `widen`/`nonEmptyVsEmpty` are the two Monotype panics
    (solve.zig `unifyTagRows`, `unifyRowWithEmpty`); in the checker they are ordinary type mismatches. -/
inductive UErr where
  | mismatch (why : String)
  | widen (label : String)
  | nonEmptyVsEmpty
  | boundedGainsTag (label : String)
  | occurs
  deriving Repr, BEq, Inhabited

def UErr.show : UErr → String
  | .mismatch w => s!"mismatch: {w}"
  | .widen l => s!"widened a closed tag union with {l}"
  | .nonEmptyVsEmpty => "unified a non-empty tag union with an empty tag union"
  | .boundedGainsTag l => s!"bounded (annotation) row gained tag {l}"
  | .occurs => "occurs check"

/-- Which unifier is running. The checker reports mismatches as user errors;
    Monotype (`InstGraph`) treats the same events as compiler bugs. -/
inductive Mode where
  | checker
  | mono
  deriving Repr, BEq, Inhabited

/-- One nominal declaration: parameters and the backing written with `param i` placeholders. -/
inductive BTy where
  | param (i : Nat)
  | str | u64
  | tags (ts : List (String × List BTy))   -- closed backing union
  | record (fs : List (String × BTy))     -- closed backing record
  deriving Repr, BEq, Inhabited

structure NomDecl where
  name    : String
  arity   : Nat
  backing : BTy
  /-- methods defined on the nominal: name and arity (receiver included) -/
  methods : List (String × Nat) := []
  deriving Repr, Inhabited

structure Store where
  slots : Array Slot := #[]
  level : Nat := 1
  noms  : List NomDecl := []
  deriving Inhabited

abbrev UM := StateT Store (Except UErr)

def fresh (c : Content) : UM Nat := do
  let s ← get
  let n := s.slots.size
  set { s with slots := s.slots.push { c := c, level := s.level } }
  return n

def freshFlex : UM Nat := fresh (.flex {})
def mkStruct (ft : FT) : UM Nat := fresh (.struct ft {})

partial def find (n : Nat) : UM Nat := do
  match (← get).slots[n]!.c with
  | .link m => find m
  | _ => return n

def content (n : Nat) : UM Content := do
  return (← get).slots[(← find n)]!.c

def setContent (n : Nat) (c : Content) : UM Unit := do
  let r ← find n
  modify fun s => { s with slots := s.slots.modify r (fun sl => { sl with c := c }) }

def levelOf (n : Nat) : UM Nat := do return (← get).slots[(← find n)]!.level

def setLevel (n : Nat) (l : Nat) : UM Unit := do
  let r ← find n
  modify fun s => { s with slots := s.slots.modify r (fun sl => { sl with level := l }) }

def marksOf : Content → Marks
  | .flex m | .rigid _ m | .struct _ m => m
  | .link _ => {}

def mergeMarks (a b : Marks) : Marks :=
  { bounded := a.bounded || b.bounded, uncommitted := a.uncommitted && b.uncommitted,
    reqs := a.reqs ++ b.reqs.filter (fun r => !(a.reqs.any (·.meth == r.meth))) }

/-- Link `a` into `b` (b's root survives with content `c`). -/
def linkTo (a b : Nat) (c : Content) : UM Unit := do
  let ra ← find a; let rb ← find b
  if ra == rb then setContent rb c else
  let la ← levelOf ra; let lb ← levelOf rb
  modify fun s =>
    let sl1 := s.slots.modify ra (fun sl => { sl with c := .link rb })
    let sl2 := sl1.modify rb (fun sl => { sl with c := c, level := min la lb })
    { s with slots := sl2 }

/-- Gather a tag row: head tags along the ext chain, and the final ext root. -/
partial def gatherTags (n : Nat) : UM (List (String × List Nat) × Nat) := do
  let r ← find n
  match (← content r) with
  | .struct (.tags ts ext) _ => do
      let (more, e) ← gatherTags ext
      return (ts ++ more, e)
  | _ => return ([], r)

partial def gatherFields (n : Nat) : UM (List (String × Nat) × Nat) := do
  let r ← find n
  match (← content r) with
  | .struct (.record fs ext) _ => do
      let (more, e) ← gatherFields ext
      return (fs ++ more, e)
  | _ => return ([], r)

def isEmptyTags (n : Nat) : UM Bool := do
  match (← content n) with
  | .struct .emptyTags _ => return true
  | _ => return false

partial def occurs (v : Nat) (n : Nat) : UM Bool := do
  let r ← find n
  if r == v then return true
  match (← content r) with
  | .struct ft _ =>
    match ft with
    | .fn as ret => do
        for a in as do if (← occurs v a) then return true
        occurs v ret
    | .record fs e => do
        for (_, a) in fs do if (← occurs v a) then return true
        occurs v e
    | .tags ts e => do
        for (_, as) in ts do for a in as do if (← occurs v a) then return true
        occurs v e
    | .nom _ as => do
        for a in as do if (← occurs v a) then return true
        return false
    | .list e => occurs v e
    | _ => return false
  | _ => return false

def nomDecl (name : String) : UM NomDecl := do
  match (← get).noms.find? (·.name == name) with
  | some d => return d
  | none => throw (.mismatch s!"unknown nominal {name}")

/-- Build a nominal's backing at the given argument vars (unify.zig `openNominalBacking`). -/
partial def openBacking (d : NomDecl) (args : List Nat) : UM Nat := do
  let rec go : BTy → UM Nat
    | .param i => return args[i]!
    | .str => mkStruct .str
    | .u64 => mkStruct .u64
    | .tags ts => do
        let mut out := []
        for (c, ps) in ts do
          let mut vs := []
          for p in ps do vs := vs ++ [← go p]
          out := out ++ [(c, vs)]
        let e ← mkStruct .emptyTags
        mkStruct (.tags out e)
    | .record fs => do
        let mut out := []
        for (f, p) in fs do out := out ++ [(f, ← go p)]
        let e ← mkStruct .emptyRec
        mkStruct (.record out e)
  go d.backing

/-- R-Lift liftability (unify.zig `tagRowLiftable`): an anonymous row lifts into a nominal when its
    tail is an unbounded flex, or an `[]` that inference closed (uncommitted). -/
def tagRowLiftable (n : Nat) : UM Bool := do
  let (_, e) ← gatherTags n
  match (← content e) with
  | .flex m => return !m.bounded
  | .struct .emptyTags m => return m.uncommitted
  | _ => return false

mutual

partial def unify (mode : Mode) (a b : Nat) : UM Unit := do
  let ra ← find a; let rb ← find b
  if ra == rb then return
  let ca ← content ra; let cb ← content rb
  match ca, cb with
  | .flex ma, .flex mb => linkTo ra rb (.flex (mergeMarks ma mb))
  | .flex ma, _ => bindVar mode ra ma rb cb
  | _, .flex mb => bindVar mode rb mb ra ca
  | .rigid x _, .rigid y _ => if x == y then linkTo ra rb cb else throw (.mismatch s!"rigid {x} vs {y}")
  | .rigid x _, _ => throw (.mismatch s!"rigid {x} vs structure")
  | _, .rigid y _ => throw (.mismatch s!"rigid {y} vs structure")
  | .struct fa ma, .struct fb mb => unifyStruct mode ra fa ma rb fb mb
  | _, _ => throw (.mismatch "link")

/-- R-Bounded (unify.zig `refuseTagsIntoBoundedExt`): a bounded tail never gains a tag.
    In Monotype the bound is not read at all (RULES.md, checker and Monotype divergence 3). -/
partial def bindVar (mode : Mode) (v : Nat) (m : Marks) (other : Nat) (oc : Content) : UM Unit := do
  if (← occurs v other) then throw .occurs
  if mode == .checker && m.bounded then
    let (ts, _) ← gatherTags other
    match ts with
    | (l, _) :: _ => throw (.boundedGainsTag l)
    | [] => pure ()
  -- requirements move onto the other side: keep them as marks on the struct so the
  -- dispatch classifier can see them; for flex/rigid merge marks.
  let oc' := match oc with
    | .struct ft mo => .struct ft (mergeMarks mo { m with bounded := false, uncommitted := false })
    | .rigid x mo => .rigid x (mergeMarks mo m)
    | c => c
  linkTo v other oc'

partial def unifyStruct (mode : Mode) (ra : Nat) (fa : FT) (ma : Marks) (rb : Nat) (fb : FT) (mb : Marks) : UM Unit := do
  let keep (ft : FT) := linkTo ra rb (.struct ft (mergeMarks ma mb))
  match fa, fb with
  | .str, .str => keep .str
  | .u64, .u64 => keep .u64
  | .emptyRec, .emptyRec => keep .emptyRec
  | .emptyTags, .emptyTags => keep .emptyTags
  | .list x, .list y => do keep (.list y); unify mode x y
  | .fn as r, .fn bs s => do
      if as.length != bs.length then throw (.mismatch "fn arity")
      keep (.fn bs s)
      for (x, y) in as.zip bs do unify mode x y
      unify mode r s
  | .nom n xs, .nom m ys => do
      if n != m || xs.length != ys.length then throw (.mismatch s!"nominal {n} vs {m}")
      keep (.nom m ys)
      for (x, y) in xs.zip ys do unify mode x y
  | .nom n xs, .tags _ _ => liftInto mode ra n xs rb
  | .tags _ _, .nom n ys => liftInto mode rb n ys ra
  | .tags _ _, .tags _ _ => unifyTagRows mode ra rb
  | .tags _ _, .emptyTags => unifyRowWithEmpty mode ra rb
  | .emptyTags, .tags _ _ => unifyRowWithEmpty mode rb ra
  | .record _ _, .record _ _ => unifyRecords mode ra rb
  | .record _ _, .emptyRec => do
      let (fs, e) ← gatherFields ra
      if !fs.isEmpty then throw (.mismatch "record with fields vs {}")
      unify mode e rb
  | .emptyRec, .record _ _ => unifyStruct mode rb fb mb ra fa ma
  | _, _ => throw (.mismatch s!"constructor clash")

/-- R-Lift (unify.zig `unifyTagUnionWithNominal`): the nominal wins; its backing is
    unified with the anonymous row. Monotype has no liftability gate (solve.zig `unifyThroughBacking`). -/
partial def liftInto (mode : Mode) (nomVar : Nat) (n : String) (args : List Nat) (anon : Nat) : UM Unit := do
  if mode == .checker then
    if !(← tagRowLiftable anon) then throw (.mismatch s!"row not liftable into {n}")
  let d ← nomDecl n
  let backing ← openBacking d args
  -- Snapshot the anonymous row before it is redirected to the nominal.
  let (ts, ext) ← gatherTags anon
  let copy ← mkStruct (.tags ts ext)
  let ma := marksOf (← content anon)
  linkTo anon nomVar (.struct (.nom n args) (mergeMarks (marksOf (← content nomVar)) ma))
  unify mode backing copy

/-- R-TagRows (unify.zig `unifyTwoTagUnions`; solve.zig `unifyTagRows`). -/
partial def unifyTagRows (mode : Mode) (a b : Nat) : UM Unit := do
  let (ta, ea) ← gatherTags a
  let (tb, eb) ← gatherTags b
  let inB (l : String) := tb.any (·.1 == l)
  let inA (l : String) := ta.any (·.1 == l)
  let onlyA := ta.filter (fun p => !inB p.1)
  let onlyB := tb.filter (fun p => !inA p.1)
  -- fail fast: a closed side cannot take the other side's extra labels
  if (← isEmptyTags ea) then
    if let (l, _) :: _ := onlyB then
      throw (if mode == .mono then .widen l else .mismatch s!"closed row lacks {l}")
  if (← isEmptyTags eb) then
    if let (l, _) :: _ := onlyA then
      throw (if mode == .mono then .widen l else .mismatch s!"closed row lacks {l}")
  -- relate the two row heads: make a's root point at b (b keeps the merged row below)
  let ma := marksOf (← content a)
  let mb := marksOf (← content b)
  let ra ← find a; let rb ← find b
  -- keep b's structure for now; payloads and tails are related below
  let cb ← content rb
  linkTo ra rb (match cb with | .struct ft _ => .struct ft (mergeMarks ma mb) | c => c)
  for (l, ps) in ta do
    match tb.find? (·.1 == l) with
    | some (_, qs) =>
        if ps.length != qs.length then throw (.mismatch s!"payload arity of {l}")
        for (x, y) in ps.zip qs do unify mode x y
    | none => pure ()
  match onlyA, onlyB with
  | [], [] => unify mode ea eb
  | _, [] => do let r ← mkStruct (.tags onlyA ea); unify mode eb r
  | [], _ => do let r ← mkStruct (.tags onlyB eb); unify mode ea r
  | _, _ => do
      -- both extend: one fresh plain flex (unify.zig `unifyTwoTagUnions`). Monotype's fresh rest
      -- defaults to [] at seal (solve.zig `joinRowRests`), modeled by `rowDefault` in Replay.
      let eps ← freshFlex
      let r1 ← mkStruct (.tags onlyB eps)
      unify mode ea r1
      let r2 ← mkStruct (.tags onlyA eps)
      unify mode eb r2

/-- R-RowEmpty (solve.zig `unifyRowWithEmpty`; unify.zig `unifyFlatType`). -/
partial def unifyRowWithEmpty (mode : Mode) (row empty : Nat) : UM Unit := do
  let (ts, e) ← gatherTags row
  if !ts.isEmpty then
    throw (if mode == .mono then .nonEmptyVsEmpty else .mismatch "tags vs []")
  unify mode e empty

/-- R-Records (unify.zig `unifyTwoRecords`): no closed fast-fail; extra fields flow into the other tail. -/
partial def unifyRecords (mode : Mode) (a b : Nat) : UM Unit := do
  let (fa, ea) ← gatherFields a
  let (fb, eb) ← gatherFields b
  let onlyA := fa.filter (fun p => !(fb.any (·.1 == p.1)))
  let onlyB := fb.filter (fun p => !(fa.any (·.1 == p.1)))
  let ra ← find a; let rb ← find b
  let cb ← content rb
  linkTo ra rb cb
  for (l, x) in fa do
    match fb.find? (·.1 == l) with
    | some (_, y) => unify mode x y
    | none => pure ()
  match onlyA, onlyB with
  | [], [] => unify mode ea eb
  | _, [] => do let r ← mkStruct (.record onlyA ea); unify mode eb r
  | [], _ => do let r ← mkStruct (.record onlyB eb); unify mode ea r
  | _, _ => do
      let eps ← freshFlex
      let r1 ← mkStruct (.record onlyB eps)
      unify mode ea r1
      let r2 ← mkStruct (.record onlyA eps)
      unify mode eb r2

end

/-! Printing types (for diagnostics and request keys). -/

partial def showTy (n : Nat) (fuel : Nat := 40) : UM String := do
  if fuel == 0 then return "…"
  let r ← find n
  match (← content r) with
  | .flex m => return s!"'{r}{if m.bounded then "b" else ""}{if m.reqs.isEmpty then "" else "{" ++ ",".intercalate (m.reqs.map (·.meth)) ++ "}"}"
  | .rigid x _ => return x
  | .link _ => return "?"
  | .struct ft m =>
    match ft with
    | .str => return "Str"
    | .u64 => return "U64"
    | .emptyRec => return "{}"
    | .emptyTags => return (if m.uncommitted then "[]u" else "[]")
    | .list e => return s!"List({← showTy e (fuel-1)})"
    | .fn as ret => do
        let xs ← as.mapM (showTy · (fuel-1))
        return s!"({", ".intercalate xs} -> {← showTy ret (fuel-1)})"
    | .nom nm as => do
        let xs ← as.mapM (showTy · (fuel-1))
        return if xs.isEmpty then nm else s!"{nm}({", ".intercalate xs})"
    | .record _ _ => do
        let (fs, e) ← gatherFields r
        let xs ← fs.mapM (fun (f, t) => do return s!"{f}: {← showTy t (fuel-1)}")
        return s!"\{{", ".intercalate xs} | {← showTy e (fuel-1)}}"
    | .tags _ _ => do
        let (ts, e) ← gatherTags r
        let xs ← ts.mapM (fun (c, ps) => do
          if ps.isEmpty then return c
          let ys ← ps.mapM (showTy · (fuel-1))
          return s!"{c}({", ".intercalate ys})")
        return s!"[{", ".intercalate xs} | {← showTy e (fuel-1)}]"

end RE
