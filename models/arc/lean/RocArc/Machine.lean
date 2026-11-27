/-
  RocArc.Machine: executable small-step semantics with a refcounted heap.

  * Values are scalars (possibly unknown), handles to heap cells, inline
    aggregates (structs and tags, which LIR keeps by value), or `uninit`.
  * A cell has a count; count 0 is a static (immortal) cell: `inc`/`dec` leave
    it alone and it is never unique.
  * `dec` to zero frees the cell and recursively `dec`s its children.
  * Touching a dead cell (count update, or a low-level op that reads its
    allocation) is a use-after-free; `dec` of a dead cell is a double free;
    a live non-static cell at the root's return is a leak.
  * A `unique_args` stamp makes the op take its in-place path without the
    runtime `count == 1` check. In `Mode.stamped` a stamp on a cell whose
    count is not 1 is a `sharedMutation` violation (the in-place write would
    be visible through another handle, or would write static data). In
    `Mode.checked` stamps are ignored, so every op takes the runtime branch:
    the reference semantics for the `unique_args` equivalence theorem.

  Unknown scalars make `switch` nondeterministic; `explore` walks every
  successor up to a step and path budget.
-/
import Std.Data.HashMap
import RocArc.Syntax
import RocArc.OpTable

namespace RocArc

inductive Val where
  | scalar (n : Option Int)
  | ptr (a : Nat)
  | agg (tag : Option (Nat × Nat)) (fields : List Val)   -- tag = (variant, discriminant)
  | uninit
  deriving Repr, Inhabited

inductive CellKind where
  | list | str | box | erased | other
  deriving Repr, Inhabited, BEq

structure Cell where
  rc : Nat                 -- 0 = static
  live : Bool := true
  kind : CellKind
  children : Array Val := #[]
  origin : String
  /-- R-Reuse: an in-place write moved this cell's identity to a new address.
      The old address is dead; any later touch through it is STALE-REUSE. -/
  reusedAs : Option Nat := none
  deriving Repr, Inhabited

abbrev Heap := Array Cell

inductive Violation where
  | useAfterFree (addr : Nat) (site : String)
  | doubleFree (addr : Nat) (site : String)
  | leak (addrs : List (Nat × String)) (site : String)
  | sharedMutation (addr rc : Nat) (site : String)
  | staleReuse (addr newAddr : Nat) (site : String)
  deriving Repr, Inhabited

def Violation.render : Violation → String
  | .useAfterFree a s => s!"USE-AFTER-FREE of cell c{a} at {s}"
  | .doubleFree a s => s!"DOUBLE-FREE of cell c{a} at {s}"
  | .leak as s => s!"LEAK at {s}: " ++ ", ".intercalate (as.map fun (a, o) => s!"c{a}({o})")
  | .sharedMutation a rc s => s!"SHARED-MUTATION: unique_args stamp on cell c{a} with count {rc} at {s}"
  | .staleReuse a b s => s!"STALE-REUSE: cell c{a} was reused in place (now c{b}) but is observed through an older reference at {s}"

inductive Mode where
  | stamped   -- honor `unique_args` (what the backends do)
  | checked   -- ignore stamps: always the runtime count==1 branch
  | pure      -- R-Pure: never write in place, never free; the un-optimized value semantics
  deriving BEq, Repr

/-- Why a path stopped. -/
inductive Halt where
  | done (v : Val)
  | violation (v : Violation)
  | crashed                       -- `crash` / `runtime_error`: a legal end of a path
  | unsupported (what : String)   -- the model does not interpret this
  | stuck (what : String)         -- ill-formed input (α or model error)
  | fuel
  deriving Inhabited

abbrev Env := Std.HashMap Local Val

structure Frame where
  proc : Proc
  env : Env := {}
  joins : Std.HashMap JoinId Stmt := {}
  conts : List Stmt := []          -- pending switch continuations
  deriving Inhabited

structure Config where
  ctrl : Stmt
  frame : Frame
  stack : List (Frame × Local × Stmt) := []   -- caller frame, result local, rest
  heap : Heap := #[]
  steps : Nat := 0
  forks : Nat := 0     -- unknown-scalar switches this path has split on
  stamps : Nat := 0    -- unique_args-stamped ops executed on this path
  trail : List String := []   -- calls, returns and switch choices (newest first)
  deriving Inhabited

structure Ctx where
  prog : Program
  mode : Mode := .stamped
  ops : Std.HashMap String RcEffect
  /-- Split on at most this many unknown-scalar switches per path; past it a
      path follows the first alternative only. -/
  forkLimit : Nat := 8
  /-- Record every RC statement and op result on the path (`--trace`). -/
  trace : Bool := false

abbrev M := ExceptT Halt (StateM Heap)

def site (f : Frame) (what : String) : String := s!"{f.proc.name}(p{f.proc.id}) {what}"

def getCell (a : Nat) : M Cell := do
  match (← get)[a]? with
  | some c => pure c
  | none => throw (.stuck s!"dangling address c{a}")

def setCell (a : Nat) (c : Cell) : M Unit := modify fun h => h.set! a c

/-- A touch of a dead cell: STALE-REUSE if it died by an in-place reuse. -/
def deadTouch (a : Nat) (c : Cell) (s : String) (dbl : Bool := false) : Violation :=
  match c.reusedAs with
  | some b => .staleReuse a b s
  | none => if dbl then .doubleFree a s else .useAfterFree a s

/-- R-Reuse: an in-place write. The cell keeps its count and contents under a
    fresh address; every reference that existed before (other than the one
    the op consumed and returns) now names a dead address. -/
def forward (a : Nat) : M Nat := do
  let c ← getCell a
  let h ← get
  set (h.push { c with origin := c.origin })
  setCell a { c with live := false, reusedAs := some h.size }
  pure h.size

def alloc (kind : CellKind) (children : Array Val) (origin : String) (rc : Nat := 1) : M Nat := do
  let h ← get
  set (h.push { rc, kind, children, origin })
  pure h.size

/-- Handles reachable from a value without entering the heap. -/
partial def Val.handles : Val → List Nat
  | .ptr a => [a]
  | .agg _ fs => fs.flatMap Val.handles
  | _ => []

/-- `inc` a handle `n` times. -/
def incAddr (s : String) (a : Nat) (n : Nat) : M Unit := do
  let c ← getCell a
  if !c.live then throw (.violation (deadTouch a c s))
  if c.rc == 0 then return
  setCell a { c with rc := c.rc + n }

/-- `dec` a handle; frees at zero and decs the children. -/
partial def decAddr (s : String) (a : Nat) : M Unit := do
  let c ← getCell a
  if !c.live then throw (.violation (deadTouch a c s true))
  if c.rc == 0 then return
  if c.rc == 1 then
    setCell a { c with rc := 0, live := false }
    for ch in c.children do
      for b in ch.handles do decAddr s b
  else setCell a { c with rc := c.rc - 1 }

def incVal (s : String) (v : Val) (n : Nat) : M Unit := do
  for a in v.handles do incAddr s a n

def decVal (s : String) (v : Val) : M Unit := do
  for a in v.handles do decAddr s a

/-- A low-level op reads the allocation behind each top-level handle. -/
def requireLive (s : String) (v : Val) : M Unit := do
  for a in v.handles do
    let c ← getCell a
    if !c.live then throw (.violation (deadTouch a c s))

/-- Releases inside an op; R-Pure never frees. -/
def decValM (ctx : Ctx) (s : String) (v : Val) : M Unit :=
  if ctx.mode == .pure then pure () else decVal s v
def decAddrM (ctx : Ctx) (s : String) (a : Nat) : M Unit :=
  if ctx.mode == .pure then pure () else decAddr s a

def scalarOf : Val → Option Int
  | .scalar n => n
  | .agg (some (_, d)) [] => some d
  | .agg (some (_, d)) _ => some d
  | _ => none

def firstHandle (v : Val) : Option Nat := v.handles.head?

def boolVal (b : Option Bool) : Val :=
  match b with
  | some true => .scalar (some 1)
  | some false => .scalar (some 0)
  | none => .scalar none

def arith (op : String) (xs : List (Option Int)) : Option Int :=
  match op, xs with
  | "num_int_add_wrap", [some a, some b] | "num_int_add_crash_on_overflow", [some a, some b]
  | "num_int_add_proven_cannot_overflow", [some a, some b] | "num_plus", [some a, some b] => some (a + b)
  | "num_int_sub_wrap", [some a, some b] | "num_int_sub_crash_on_overflow", [some a, some b]
  | "num_int_sub_proven_cannot_overflow", [some a, some b] | "num_minus", [some a, some b] => some (a - b)
  | "num_int_mul_wrap", [some a, some b] | "num_int_mul_crash_on_overflow", [some a, some b]
  | "num_times", [some a, some b] => some (a * b)
  | "num_is_eq", [some a, some b] => some (if a == b then 1 else 0)
  | "num_is_lt", [some a, some b] => some (if a < b then 1 else 0)
  | "num_is_lte", [some a, some b] => some (if a ≤ b then 1 else 0)
  | "num_is_gt", [some a, some b] => some (if a > b then 1 else 0)
  | "num_is_gte", [some a, some b] => some (if a ≥ b then 1 else 0)
  | "bool_not", [some a] => some (if a == 0 then 1 else 0)
  | _, _ => none

/-- The in-place-or-copy decision of a runtime-uniqueness op on handle `a`.
    Returns the handle the result lives in. -/
def uniquePath (ctx : Ctx) (s : String) (a : Nat) (stamped : Bool) (origin : String) : M Nat := do
  let c ← getCell a
  if !c.live then throw (.violation (deadTouch a c s))
  if ctx.mode == .pure then
    -- R-Pure: always a fresh copy, no count traffic
    return (← alloc c.kind c.children origin)
  let inPlace := if stamped && ctx.mode == .stamped then
      (if c.rc != 1 then none else some true)
    else some (c.rc == 1)
  match inPlace with
  | none => throw (.violation (.sharedMutation a c.rc s))
  | some true => forward a
  | some false =>
    -- copy: the new cell owns a new count on every child; the consumed handle is released
    let b ← alloc c.kind c.children origin
    for ch in c.children do incVal s ch 1
    decAddr s a
    pure b

def childAt (c : Cell) (i : Option Int) : Val :=
  match i with
  | some n => c.children[n.toNat]?.getD (.scalar none)
  | none => c.children[0]?.getD (.scalar none)

/-- α gap G2 for op results: a struct result's field layout is not printed.
    The field a procedure reads back out of `x` (through `ref.local`/`set`
    aliases) at a refcounted layout is where an op that shares an argument
    put the shared handle. -/
partial def sharedFieldOf (body : Stmt) (x : Local) : Option Nat := Id.run do
  let rec walk (s : Stmt) (al : List Local) (fuel : Nat) : List Local × Option Nat :=
    if fuel == 0 then (al, none) else
    match s with
    | .assign y _ (.ref .local_ z _) k => walk k (if al.contains z then y :: al else al) (fuel - 1)
    | .assign _ ty (.ref (.field i) z _) k =>
      if al.contains z && (ty.kind == .str || ty.kind == .list) then (al, some i) else walk k al (fuel - 1)
    | .set y z k => walk k (if al.contains z then y :: al else al) (fuel - 1)
    | .assign _ _ _ k | .inc _ _ k | .dec _ k | .free _ k | .initUninit _ k | .use _ k
    | .decIfInit _ _ _ k => walk k al (fuel - 1)
    | .switch _ cs d c =>
      let bs := cs.map (·.2) ++ [d] ++ (match c with | some c => [c] | none => [])
      bs.foldl (fun (al, r) b => match r with
        | some i => (al, some i)
        | none => walk b al (fuel - 1)) (al, none)
    | .switchInit _ _ _ a b => match walk a al (fuel - 1) with
      | (al, some i) => (al, some i)
      | (al, none) => walk b al (fuel - 1)
    | .join _ _ r b => match walk r al (fuel - 1) with
      | (al, some i) => (al, some i)
      | (al, none) => walk b al (fuel - 1)
    | _ => (al, none)
  return (walk body [x] 100000).2

/-- Semantics of one low-level op. Specific list ops are modeled exactly
    enough to keep element handles; every other op follows its RcEffect row. -/
def lowLevel (ctx : Ctx) (s : String) (op : String) (args : List Val) (unique : Nat) (resTy : Ty)
    (sharedField : Option Nat := none) : M Val := do
  let some e := ctx.ops.get? op | throw (.unsupported s!"low_level {op}: no RcEffect row")
  -- every non-representation argument's allocation must be live when the op reads it
  for (v, i) in args.zipIdx do
    if !(bit e.reprArgs i) then requireLive s v
  let arg (i : Nat) : Val := args[i]?.getD (.scalar none)
  let stamped (i : Nat) := bit unique i
  match op with
  | "list_len" =>
    match firstHandle (arg 0) with
    | some a => let c ← getCell a; pure (.scalar (some c.children.size))
    | none => pure (.scalar none)
  | "list_get_unsafe" | "list_sublist_borrowed" =>
    match firstHandle (arg 0) with
    | some a => let c ← getCell a; pure (childAt c (scalarOf (arg 1)))
    | none => pure (.scalar none)
  | "list_map_can_reuse" =>
    -- `listMapCanReuse`: not a slice and exclusively owned (count 1)
    match firstHandle (arg 0) with
    | some a => let c ← getCell a; pure (.scalar (some (if c.rc == 1 then 1 else 0)))
    | none => pure (.scalar (some 0))
  | "list_owned_unique" | "list_slack_unique" =>
    -- the runtime uniqueness check a cached-uniqueness loop reads later
    -- (`list.zig` `listOwnedUnique` / `listSlackUnique`): 0 unless the list
    -- owns an allocation with count 1; the slack's capacity is unknown
    match firstHandle (arg 0) with
    | some a =>
      let c ← getCell a
      if c.rc != 1 then pure (.scalar (some 0))
      else if op == "list_owned_unique" then pure (.scalar (some 1)) else pure (.scalar none)
    | none => pure (.scalar (some 0))
  | "list_map_prepare_reuse" =>
    -- an ownership transfer of a list `list_map_can_reuse` found unique: the
    -- reuse point (R-Reuse). In R-Pure mode the map writes into a copy.
    let some a := firstHandle (arg 0) | throw (.stuck s!"{op} on a non-list")
    let c ← getCell a
    if ctx.mode == .pure then return .ptr (← alloc c.kind c.children s)
    if c.rc == 1 then return .ptr (← forward a)
    -- not unique: the map's unconditional writes report SHARED-MUTATION
    pure (.ptr a)
  | "list_with_capacity" => pure (.ptr (← alloc .list #[] s))
  | "list_append_unsafe" | "list_prepend" =>
    let some a := firstHandle (arg 0) | throw (.stuck s!"{op} on a non-list")
    let r ← uniquePath ctx s a (stamped 0) s
    -- retain_args bit 1: in final LIR the retain is an explicit `incref` ARC
    -- emitted (or the caller's unit transfers), so the op only stores the handle
    let c ← getCell r
    let ch := if op == "list_prepend" then #[arg 1] ++ c.children else c.children.push (arg 1)
    setCell r { c with children := ch }
    pure (.ptr r)
  | "list_set" | "list_set_in_place_unsafe" | "list_replace_unsafe" | "list_map_write_unsafe" =>
    let some a := firstHandle (arg 0) | throw (.stuck s!"{op} on a non-list")
    let r ← if op == "list_set" || op == "list_replace_unsafe" then uniquePath ctx s a (stamped 0) s
      else do
        -- no runtime check: the op writes in place unconditionally
        let c ← getCell a
        -- R-Pure: a set copies; a map write goes into the buffer
        -- `list_map_prepare_reuse` already copied
        if ctx.mode == .pure then
          (if op == "list_set_in_place_unsafe" then alloc c.kind c.children s else pure a)
        else if c.rc != 1 then throw (.violation (.sharedMutation a c.rc s))
        else pure a
    let c ← getCell r
    let idx := (scalarOf (arg 1)).getD 0 |>.toNat
    let old := c.children[idx]?.getD (.scalar none)
    let ch := if idx < c.children.size then c.children.set! idx (arg 2) else c.children.push (arg 2)
    setCell r { c with children := ch }
    if op == "list_replace_unsafe" then
      pure (.agg none [.ptr r, old])   -- { list, prev }: prev's unit moves to the caller
    else
      decValM ctx s old
      pure (.ptr r)
  | "list_concat" =>
    let some a := firstHandle (arg 0) | throw (.stuck "list_concat on a non-list")
    let some b := firstHandle (arg 1) | throw (.stuck "list_concat on a non-list")
    let r ← uniquePath ctx s a (stamped 0) s
    let cb ← getCell b
    for ch in cb.children do incVal s ch 1
    decAddrM ctx s b
    let c ← getCell r
    setCell r { c with children := c.children ++ cb.children }
    pure (.ptr r)
  | _ =>
    -- Generic RcEffect semantics.
    let rcArgs := args.zipIdx
    -- 1. the in-place candidate (first consumed, aliased argument)
    let aliasIdx := rcArgs.find? fun (v, i) => bit e.aliasConsumed i && (firstHandle v).isSome
    let mut result : Option Val := none
    if let some (v, i) := aliasIdx then
      let some a := firstHandle v | throw (.stuck "unreachable")
      if bit e.uniqCheck i then
        result := some (.ptr (← uniquePath ctx s a (stamped i) s))
      else
        result := some v
    -- 2. results that alias storage kept live by an argument
    if result.isNone && (e.retainResult || e.borrowsArgs != 0) then
      let src := (rcArgs.find? fun (_, i) => bit e.borrowsArgs i).map (·.1) |>.getD (arg 0)
      match firstHandle src with
      | some a => let c ← getCell a; result := some (childAt c (scalarOf (arg 1)))
      | none => result := some (.scalar none)
    -- 3. a result sharing an argument's allocation (slices, reinterpretations, unbox)
    if result.isNone && e.sharesArgs != 0 then
      let src := (rcArgs.find? fun (_, i) => bit e.sharesArgs i).map (·.1) |>.getD (arg 0)
      match firstHandle src, (← get)[(firstHandle src).getD 0]? with
      | some a, some c =>
        if op == "box_unbox" || op == "erased_capture_load" then
          -- ARC rewrites these to their borrowed variants before final LIR
          throw (.stuck s!"{op} in final LIR (ARC replaces it with its borrowed variant)")
        else if e.retain != 0 then
          -- a slice/reinterpretation: ARC's explicit incref of the argument pays for it
          let _ := c
          result := some (.ptr a)
        else
          -- the op retains the shared allocation itself (may_retain_or_release)
          incAddr s a 1
          -- a fallible conversion (`str_from_utf8`: Ok(str) | Err) returns a
          -- tag union; only Ok carries the shared handle. The machine takes
          -- the Ok path (discriminant 1: Err < Ok), so the Err path of such an
          -- op is not explored (RULES.md, O-Shares)
          result := some (match resTy.kind, sharedField with
            | .tagUnion, _ => .agg (some (1, 1)) [.ptr a]
            -- a struct result (`*_from_str_prefix`: { …, rest }): the handle
            -- sits in the field the procedure reads back as a str or list
            | .struct_, some i => .agg none ((List.replicate i (Val.scalar none)) ++ [.ptr a])
            | _, _ => .ptr a)
      | _, _ => result := some (.scalar none)
    -- 4. retained arguments are stored in the result. ARC materializes the
    --    retain as an explicit `incref` before the op (arc.zig `retainMaskedArgs`)
    --    or transfers the caller's unit, so the op adds no count itself.
    let mut stored : Array Val := #[]
    for (v, i) in rcArgs do
      if bit e.retain i then
        stored := stored.push v
    -- 5. consumed arguments that do not live on in the result: moved into a fresh
    --    result cell, or released by the op
    let mut moved : Array Val := #[]
    for (v, i) in rcArgs do
      if bit e.consume i && !(bit e.aliasConsumed i) then
        if op == "box_unbox" || op == "erased_capture_load" then decValM ctx s v
        else moved := moved.push v
    match result with
    | some r =>
      -- stored/moved values join an in-place result cell
      if let some a := firstHandle r then
        let c ← getCell a
        -- a retained argument whose allocation *is* the result (a slice or
        -- view: `str_drop_prefix`, `str_substring_unsafe`) is shared, not
        -- stored: the retain pays for the second reference
        let kept := stored.filter fun v => firstHandle v != some a
        if kept.size + moved.size > 0 then
          setCell a { c with children := c.children ++ kept ++ moved }
      else
        for v in moved do decValM ctx s v
      pure r
    | none =>
      let isRc := match resTy.kind with
        | .str | .list | .box | .erased => true | _ => false
      if isRc && (e.mayAllocate || e.resultUnique || stored.size + moved.size > 0) then
        let kind := match resTy.kind with
          | .list => CellKind.list | .str => .str | .box => .box | .erased => .erased | _ => .other
        pure (.ptr (← alloc kind (stored ++ moved) s))
      else if isRc then
        -- an op whose row promises no allocation and no sharing returns an
        -- immortal value (empty / small string)
        pure (.ptr (← alloc .other #[] s!"{s} (no-alloc result)" 0))
      else
        for v in moved do decValM ctx s v
        if resTy.kind == .struct_ || resTy.kind == .tagUnion then
          pure (.agg none [])
        else
          pure (.scalar (arith op (args.map scalarOf)))

def look (f : Frame) (x : Local) : M Val :=
  match f.env.get? x with
  | some v => pure v
  | none => throw (.stuck s!"{f.proc.name}: read of unbound local l{x}")

def fieldOf (v : Val) (i : Nat) : Val :=
  match v with
  | .agg _ fs => fs[i]?.getD (.scalar none)
  | _ => .scalar none

def staticLiteral (origin : String) (len : Option Nat) : M Val := do
  let children := Array.replicate (len.getD 0) (Val.scalar none)
  pure (.ptr (← alloc .other children origin 0))

/-- A right-hand side: a value, or a call to enter. -/
def evalExpr (ctx : Ctx) (f : Frame) (x : Local) (ty : Ty) (e : Expr) : M (Val ⊕ (Proc × List Val)) := do
  let s := site f s!"l{x}"
  match e with
  | .ref k src _ =>
    let v ← look f src
    let r := match k with
      | .local_ | .listReinterpret | .nominal => v
      | .discriminant => match v with
        | .agg (some (_, d)) _ => .scalar (some d)
        | _ => .scalar none
      | .field i => fieldOf v i
      | .tagPayloadStruct _ => match v with
        | .agg _ [p] => p
        | _ => .scalar none
      | .tagPayload _ i => match v with
        | .agg _ [p] => (match p with
          | .agg none _ => fieldOf p i
          | _ => if i == 0 then p else .scalar none)
        | _ => .scalar none
    pure (.inl r)
  | .lit l len => match l with
    | .int n => pure (.inl (.scalar (some n)))
    | .float | .procRef _ => pure (.inl (.scalar none))
    | .str => pure (.inl (← staticLiteral s!"{s} literal str" none))
    | .bytes => pure (.inl (← staticLiteral s!"{s} literal bytes" len))
    | .staticData id => pure (.inl (← staticLiteral s!"{s} static_data s{id}" len))
  | .call p args =>
    let some callee := ctx.prog.find? p | throw (.unsupported s!"call of p{p}, which the dump does not contain")
    let vs ← args.mapM (look f)
    pure (.inr (callee, vs))
  | .lowLevel op args uniq =>
    let vs ← args.mapM (look f)
    let sf := if ty.kind == .struct_ then sharedFieldOf f.proc.body x else none
    pure (.inl (← lowLevel ctx s op vs uniq ty sf))
  | .list elems =>
    let vs ← elems.mapM (look f)
    pure (.inl (.ptr (← alloc .list vs.toArray s)))
  | .struct_ fs =>
    let vs ← fs.mapM (look f)
    pure (.inl (.agg none vs))
  | .tag v d payload =>
    match payload with
    | some p => pure (.inl (.agg (some (v, d)) [← look f p]))
    | none => pure (.inl (.agg (some (v, d)) []))
  | .unsupported t => throw (.unsupported t)

/-- The cells a value reaches directly, with their counts, for `--trace`. -/
partial def cellsOf (h : Heap) : Option Val → String
  | none => ""
  | some v =>
    let rec hs : Val → List Nat
      | .ptr a => [a]
      | .agg _ fs => fs.flatMap hs
      | _ => []
    let parts := (hs v).map fun a => match h[a]? with
      | some c => s!"c{a}={if c.live then toString c.rc else "freed"}" ++
          (let ks := c.children.toList.flatMap hs; if ks.isEmpty then "" else s!"⟨children {ks}⟩")
      | none => s!"c{a}=?"
    " ".intercalate parts

/-- Successor configurations, or a halt. -/
def step (ctx : Ctx) (cfg : Config) : Except Halt (List Config) :=
  let f := cfg.frame
  let run {α} (m : M α) : Except Halt (α × Heap) :=
    match (m.run).run cfg.heap with
    | (.ok a, h) => .ok (a, h)
    | (.error e, _) => .error e
  let next (ctrl : Stmt) (f' : Frame) (h : Heap) : Config :=
    { cfg with ctrl, frame := f', heap := h, steps := cfg.steps + 1 }
  -- R-Pure: count statements have no effect
  let rcNext : Option Stmt := match cfg.ctrl with
    | .inc _ _ k | .dec _ k | .free _ k | .decIfInit _ _ _ k => some k
    | _ => none
  if ctx.mode == .pure && rcNext.isSome then .ok [next rcNext.get! f cfg.heap] else
  match cfg.ctrl with
  | .assign x ty e k =>
    let stamped := match e with | .lowLevel _ _ u => u != 0 | _ => false
    match (evalExpr ctx f x ty e |>.run).run cfg.heap with
    | (.ok (.inl v), h) =>
      let c := next k { f with env := f.env.insert x v } h
      let c := if ctx.trace && (match e with | .lowLevel .. => true | _ => false) && !(cellsOf h (some v)).isEmpty then
        { c with trail := s!"{f.proc.name}: l{x}={match e with | .lowLevel op _ _ => op | _ => ""} {cellsOf h (some v)}" :: c.trail } else c
      .ok [if stamped then { c with stamps := c.stamps + 1 } else c]
    | (.error halt, _) => .error halt
    | (.ok (.inr (callee, vs)), h) =>
      if callee.params.length != vs.length then .error (.stuck s!"arity mismatch calling p{callee.id}") else
      let env : Env := (callee.params.zip vs).foldl (fun m ((p, _), v) => m.insert p v) {}
      let fr : Frame := { proc := callee, env := env }
      let tr := s!"call {callee.name}(p{callee.id})" :: cfg.trail
      .ok [{ cfg with ctrl := callee.body, frame := fr, stack := (f, x, k) :: cfg.stack, heap := h, steps := cfg.steps + 1, trail := tr }]
  | .set x y k => do
    match f.env.get? y with
    | some v => .ok [next k { f with env := f.env.insert x v } cfg.heap]
    | none => .error (.stuck s!"{f.proc.name}: set from unbound l{y}")
  | .inc x n k => do
    let (_, h) ← run (do incVal (site f s!"incref l{x}") (← look f x) n)
    let c := next k f h
    .ok [if ctx.trace then { c with trail := s!"{f.proc.name}: inc l{x} {cellsOf h (f.env.get? x)}" :: c.trail } else c]
  | .dec x k | .free x k => do
    let before := cellsOf cfg.heap (f.env.get? x)
    let at_ := if ctx.trace then s!"decref l{x} [before: {before}; value: {reprStr (f.env.get? x)}]" else s!"decref l{x}"
    let (_, h) ← run (do decVal (site f at_) (← look f x))
    let c := next k f h
    .ok [if ctx.trace then { c with trail := s!"{f.proc.name}: dec l{x} {before}→{cellsOf h (f.env.get? x)}" :: c.trail } else c]
  | .decIfInit _ _ x k => do
    match f.env.get? x with
    | none | some .uninit => .ok [next k f cfg.heap]
    | some v =>
      let (_, h) ← run (decVal (site f s!"decref_if_initialized l{x}") v)
      .ok [next k f h]
  | .initUninit x k => .ok [next k { f with env := f.env.insert x .uninit } cfg.heap]
  | .use x k => do
    let (_, h) ← run (do requireLive (site f s!"use l{x}") (← look f x))
    .ok [next k f h]
  | .switch x cases d cont => do
    let f' := match cont with
      | some c => { f with conts := c :: f.conts }
      | none => f
    match f.env.get? x with
    | none => .error (.stuck s!"{f.proc.name}: switch on unbound l{x}")
    | some v =>
      let tr (c : Config) (what : String) : Config := { c with trail := s!"switch l{x}={what}" :: c.trail }
      match scalarOf v with
      | some n => match cases.find? (·.1 == n) with
        | some (_, b) => .ok [tr (next b f' cfg.heap) s!"{n}"]
        | none => .ok [tr (next d f' cfg.heap) s!"{n}"]
      | none =>
        let alts := (cases.map fun (_, b) => b) ++ [d]
        if cfg.forks < ctx.forkLimit then
          .ok ((alts.zipIdx).map fun (b, i) => tr { next b f' cfg.heap with forks := cfg.forks + 1 } s!"?alt{i}")
        else .ok [tr (next alts.head! f' cfg.heap) "?alt0(limit)"]
  | .switchInit _ _ p a b =>
    match f.env.get? p with
    | none | some .uninit => .ok [next b f cfg.heap]
    | some _ => .ok [next a f cfg.heap]
  | .join j _ rem body => .ok [next rem { f with joins := f.joins.insert j body } cfg.heap]
  | .jump j =>
    match f.joins.get? j with
    | some body => .ok [next body f cfg.heap]
    | none => .error (.stuck s!"{f.proc.name}: jump to unregistered j{j}")
  | .ret x =>
    match f.env.get? x with
    | none => .error (.stuck s!"{f.proc.name}: ret of unbound l{x}")
    | some v =>
      match cfg.stack with
      | [] => .error (.done v)
      | (caller, tgt, k) :: rest =>
        .ok [{ cfg with ctrl := k, frame := { caller with env := caller.env.insert tgt v },
                        stack := rest, steps := cfg.steps + 1 }]
  | .crash _ | .runtimeError => .error .crashed
  | .unsupported t => .error (.unsupported t)

/-- Leak check at the root's return: every live, non-static cell must be
    reachable from the returned value. -/
partial def reach (h : Heap) (work : List Nat) (seen : Std.HashMap Nat Unit) : Std.HashMap Nat Unit :=
  match work with
  | [] => seen
  | a :: rest =>
    if seen.contains a then reach h rest seen else
    let kids := (h[a]?.map (·.children.toList.flatMap Val.handles)).getD []
    reach h (kids ++ rest) (seen.insert a ())

def leakCheck (cfg : Config) (v : Val) : Option Violation :=
  let seen := reach cfg.heap v.handles {}
  let leaked := (cfg.heap.toList.zipIdx).filter fun (c, i) => c.live && c.rc != 0 && !seen.contains i
  -- report only leak roots: cells no other leaked cell points to
  let pointed := leaked.flatMap fun (c, _) => c.children.toList.flatMap Val.handles
  let roots := leaked.filter fun (_, i) => !pointed.contains i
  let roots := if roots.isEmpty then leaked else roots
  if leaked.isEmpty then none
  else some (.leak (roots.map fun (c, i) => (i, c.origin)) (site cfg.frame "ret"))

/-- R-Equiv: a value's observable contents, following handles into the heap.
    Unknown scalars print `?`; a dead cell prints `<dead>`. -/
partial def render (h : Heap) (fuel : Nat) : Val → String
  | .scalar (some n) => toString n
  | .scalar none => "?"
  | .uninit => "uninit"
  | .ptr a =>
    if fuel == 0 then "…" else
    match h[a]? with
    | some c => if !c.live then "<dead>" else
        "[" ++ ",".intercalate (c.children.toList.map (render h (fuel - 1))) ++ "]"
    | none => "<dangling>"
  | .agg t fs =>
    let pre := match t with | some (v, _) => s!"T{v}" | none => ""
    pre ++ "{" ++ ",".intercalate (fs.map (render h fuel)) ++ "}"

/-- R-Equiv's comparison: `?` (an unknown scalar, which the two modes can
    reach by different but equivalent routes, e.g. a uniqueness query answering
    differently under R-Pure's never-freed counts) matches anything. -/
partial def compatObs (a b : List Char) : Bool :=
  -- skip one token on the side that is `?`
  let rec skipTok (cs : List Char) (depth : Nat) : List Char :=
    match cs with
    | [] => []
    | c :: rest =>
      if c == '[' || c == '{' then skipTok rest (depth + 1)
      else if (c == ']' || c == '}') then (if depth == 0 then cs else if depth == 1 then rest else skipTok rest (depth - 1))
      else if c == ',' && depth == 0 then cs
      else skipTok rest depth
  match a, b with
  | [], [] => true
  | '?' :: ra, _ => compatObs ra (skipTok b 0)
  | _, '?' :: rb => compatObs (skipTok a 0) rb
  | x :: ra, y :: rb => x == y && compatObs ra rb
  | _, _ => false

structure Report where
  results : Array String := #[]   -- R-Equiv: rendered value of each finished path, in exploration order
  paths : Nat := 0
  done : Nat := 0
  crashed : Nat := 0
  violations : Array Violation := #[]
  unsupported : Array String := #[]
  stuck : Array String := #[]
  outOfFuel : Nat := 0
  truncated : Bool := false
  stampsExecuted : Nat := 0
  firstTrail : List String := []
  violationTrail : List String := []   -- the path of the first violation
  deriving Inhabited

/-- Depth-first exploration of every path from `start`, bounded by
    `maxSteps` per path and `maxPaths` overall. -/
partial def explore (ctx : Ctx) (start : Config) (maxSteps maxPaths : Nat) : Report := Id.run do
  let mut work : List Config := [start]
  let mut r : Report := {}
  while true do
    match work with
    | [] => break
    | cfg :: rest =>
      work := rest
      if r.paths ≥ maxPaths then
        r := { r with truncated := true }
        break
      if cfg.steps ≥ maxSteps then
        r := { r with paths := r.paths + 1, outOfFuel := r.outOfFuel + 1 }
        continue
      match step ctx cfg with
      | .ok succs => work := succs ++ work
      | .error h =>
        r := { r with paths := r.paths + 1, stampsExecuted := r.stampsExecuted + cfg.stamps,
                      firstTrail := if r.paths == 0 then cfg.trail.reverse else r.firstTrail }
        match h with
        | .done v =>
          r := { r with results := r.results.push (render cfg.heap 6 v) }
          match (if ctx.mode == .pure then none else leakCheck cfg v) with
          | some viol =>
            let vt := if r.violations.isEmpty then cfg.trail.reverse else r.violationTrail
            r := { r with violations := r.violations.push viol, violationTrail := vt }
          | none => r := { r with done := r.done + 1 }
        | .violation v =>
          let vt := if r.violations.isEmpty then cfg.trail.reverse else r.violationTrail
          r := { r with violations := r.violations.push v, violationTrail := vt }
        | .crashed => r := { r with crashed := r.crashed + 1 }
        | .unsupported t => r := { r with unsupported := r.unsupported.push t }
        | .stuck t => r := { r with stuck := r.stuck.push t }
        | .fuel => r := { r with outOfFuel := r.outOfFuel + 1 }
  return r

def opMap : Std.HashMap String RcEffect := opTable.foldl (fun m (n, e) => m.insert n e) {}

/-- Run a root procedure (no parameters) of a program. -/
def runRoot (P : Program) (root : Proc) (mode : Mode := .stamped)
    (maxSteps : Nat := 200000) (maxPaths : Nat := 2000) (forkLimit : Nat := 8) (trace : Bool := false) : Report :=
  let ctx : Ctx := { prog := P, mode, ops := opMap, forkLimit, trace }
  explore ctx { ctrl := root.body, frame := { proc := root } } maxSteps maxPaths

end RocArc
