/-
  RocArc.Certify: the debug borrow certifier's judgment, as an executable
  abstract interpretation (design.md "Debug Borrow Certifier", ~28671;
  `src/lir/arc_certify.zig` module header).

  Σ ⊢ p holds iff every path of p's body, walked from Σ(p)'s entry state,
  passes the rules below. Join bodies are walked once per distinct entry
  summary: the "worst case" of the certifier's join-group lattice, which the
  design states is exact (the lattice only merges summaries whose walks are
  implied by one walk under the meet partition). So this model accepts exactly
  the schedules the real certifier is designed to accept, minus the precision
  the real one has and this one lacks (listed in RULES.md under "Model gaps").

  Abstract state: each refcounted local names a value; a value has a unit
  balance, a mode (owned / borrowed-from lenders / static), the field slots
  already taken out of it, and an optional holder (the aggregate an operand's
  unit moved into). Rule names (C-…) match RULES.md.
-/
import Std.Data.HashMap
import RocArc.Syntax
import RocArc.OpTable

namespace RocArc
namespace Cert

abbrev VId := Nat

inductive Mode where
  | owned
  | borrowed (lenders : List VId)   -- live while every lender is live; [] = the caller's
  | static
  deriving Repr, BEq, Inhabited

structure VInfo where
  bal : Int
  mode : Mode
  taken : List Nat := []
  holder : Option VId := none
  slot : Option (VId × Nat) := none   -- a projection: (container value, slot)
  residual : Bool := false            -- some slot's unit was claimed out of it
  uniq : Bool := false                -- born unique and never retained since (C-Birth)
  param : Bool := false               -- a parameter's value (seeds are not in the dump)
  deriving Repr, BEq, Inhabited

structure St where
  vals : Std.HashMap Local VId := {}
  info : Std.HashMap VId VInfo := {}
  next : VId := 0
  trail : List String := []      -- most recent statements first, for failure reports
  steps : Nat := 0               -- statements walked, for the model's walk budget
  deriving Inhabited

structure Env where
  prog : Program
  proc : Proc
  rcLayouts : List Nat
  ops : Std.HashMap String RcEffect
  types : Std.HashMap Local Ty
  liveIn : Std.HashMap JoinId (List Local) := {}

abbrev CM := StateT St (Except String)

def fail {α} (rule : String) (msg : String) : CM α := do
  let t := (← get).trail.take 14 |>.reverse
  throw (s!"[{rule}] {msg}\n      path: " ++ " ; ".intercalate t : String)

def fresh (i : VInfo) : CM VId := do
  let s ← get
  set { s with info := s.info.insert s.next i, next := s.next + 1 }
  return s.next

def infoOf (v : VId) : CM VInfo := do
  match (← get).info.get? v with
  | some i => return i
  | none => fail "C-Internal" s!"unknown value v{v}"

def putInfo (v : VId) (i : VInfo) : CM Unit := modify fun s => { s with info := s.info.insert v i }

def bind (x : Local) (v : VId) : CM Unit := modify fun s => { s with vals := s.vals.insert x v }

def isRc (env : Env) (x : Local) : Bool :=
  match env.types.get? x with
  | some t => t.mayBeRc env.rcLayouts
  | none => false

/-- Liveness: a unit remains, or the value is static, or every lender is live,
    or the aggregate holding its unit is live. Fuel bounds the lender walk. -/
partial def liveV (s : St) (v : VId) (fuel : Nat := 64) : Bool :=
  if fuel == 0 then false else
  match s.info.get? v with
  | none => false
  | some i =>
    i.bal > 0 ||
    (match i.mode with
     | .static => true
     | .borrowed ls => ls.all (liveV s · (fuel - 1))
     | .owned => false) ||
    (match i.holder with
     | some h => liveV s h (fuel - 1)
     | none => false)

def valOf (x : Local) : CM VId := do
  match (← get).vals.get? x with
  | some v => return v
  | none => fail "C-Bound" s!"use of unbound refcounted local l{x}"

def requireLive (rule : String) (x : Local) : CM VId := do
  let v ← valOf x
  if !liveV (← get) v then fail rule s!"read of l{x} (v{v}) after its value died"
  return v

/-- Spend one unit of `x`'s value (a release or a move). -/
def spend (rule : String) (x : Local) (allowNegative : Bool := false) : CM VId := do
  let v ← valOf x
  let i ← infoOf v
  match i.mode with
  | .static => return v
  | _ => pure ()
  if i.bal < 1 then
    -- C-Claim: spending a projection of an owned inline container claims the
    -- container's unit for that slot (the residual shell keeps the rest).
    -- This comes first even where a transient negative balance is allowed
    -- (C-Ctor), so a payload moved into an aggregate claims its slot
    if let some (c, k) := i.slot then
      let ci ← infoOf c
      -- any held unit can be claimed, including one an `incref` added to a
      -- borrowed value (a retained element read out of a list)
      if ci.mode != .static && ci.bal ≥ 1 && !(ci.taken.contains k) then
        putInfo c { ci with taken := k :: ci.taken, residual := true }
        putInfo v { i with mode := .owned, slot := none }
        return v
      if ci.taken.contains k && !allowNegative then
        fail "C-ClaimTwice" s!"l{x} (v{v}) claims slot {k} of v{c} a second time"
  if i.bal < 1 && !allowNegative then
    fail rule s!"l{x} (v{v}) spends a unit it does not hold (balance {i.bal}, mode {reprStr i.mode})"
  putInfo v { i with bal := i.bal - 1 }
  return v

/-- A take out of a projection also claims every container above it. -/
partial def claimUp (v : VId) (fuel : Nat := 32) : CM Unit := do
  if fuel == 0 then return
  let i ← infoOf v
  putInfo v { i with residual := true }
  if let some (c, _) := i.slot then claimUp c (fuel - 1)

def certExpr (env : Env) (x : Local) (ty : Ty) (e : Expr) : CM Unit := do
  let rc := ty.mayBeRc env.rcLayouts
  match e with
  | .ref k y take =>
    if !isRc env y then
      if rc then bind x (← fresh { bal := 0, mode := .static })   -- a refcounted read of a non-rc carrier: model gap
      return
    match k with
    | .local_ | .nominal | .listReinterpret =>
      bind x (← valOf y)
    | .discriminant => discard <| requireLive "C-Read" y
    | .field i | .tagPayload _ i =>
      let v ← requireLive "C-Proj" y
      let iv ← infoOf v
      if iv.taken.contains i then fail "C-ReadAfterTake" s!"l{x} reads slot {i} of l{y} after it was taken"
      if take then
        putInfo v { iv with taken := i :: iv.taken, residual := true }
        claimUp v
        -- C-BirthTake: a take moving the only unit out of a unique container is unique
        if rc then bind x (← fresh { bal := 1, mode := .owned, uniq := iv.uniq && iv.bal == 1 })
      else if rc then
        -- a part of static data is static (immortal) itself
        if iv.mode == .static then bind x (← fresh { bal := 0, mode := .static })
        else bind x (← fresh { bal := 0, mode := .borrowed [v], slot := some (v, i) })
    | .tagPayloadStruct _ =>
      let v ← requireLive "C-Proj" y
      let iv ← infoOf v
      if iv.taken.contains 0 then fail "C-ReadAfterTake" s!"l{x} reads the payload of l{y} after it was taken"
      if take then
        putInfo v { iv with taken := 0 :: iv.taken, residual := true }
        claimUp v
        if rc then bind x (← fresh { bal := 1, mode := .owned, uniq := iv.uniq && iv.bal == 1 })
      else if rc then
        if iv.mode == .static then bind x (← fresh { bal := 0, mode := .static })
        else bind x (← fresh { bal := 0, mode := .borrowed [v], slot := some (v, 0) })
  -- C-Lit: the dump is final LIR, after ImmortalLocals elision, so a literal
  -- is image-backed (static) and carries no unit
  | .lit _ _ => if rc then bind x (← fresh { bal := 0, mode := .static })
  | .list xs | .struct_ xs =>
    if !rc && !(xs.any (isRc env)) then return
    -- C-BirthAgg: a struct is unique when every refcounted operand is
    let mut allUniq := true
    for y in xs do
      if isRc env y then
        let i ← infoOf (← valOf y)
        if !(i.uniq && i.bal == 1) then allUniq := false
    -- an inline struct of static parts owns nothing: it is static itself
    -- (a list still allocates)
    let mut allStatic := true
    for y in xs do
      if isRc env y then
        if (← infoOf (← valOf y)).mode != .static then allStatic := false
    if rc && allStatic && (e matches .struct_ _) && xs.any (isRc env) then
      bind x (← fresh { bal := 0, mode := .static }); return
    -- an aggregate of a layout not known to be refcounted is untracked (model gap)
    let agg ← fresh { bal := 1, mode := .owned, residual := !rc, uniq := (e matches .list _) || allUniq }
    for y in xs do
      if isRc env y then
        let v ← spend "C-Ctor" y (allowNegative := true)
        let i ← infoOf v
        putInfo v { i with holder := some agg }
    if rc then bind x agg
  | .tag _ _ p =>
    if !rc then return
    let pu ← match p with
      | some y => if isRc env y then do
          let i ← infoOf (← valOf y); pure (i.uniq && i.bal == 1) else pure true
      | none => pure true
    if let some y := p then
      if isRc env y then
        if (← infoOf (← valOf y)).mode == .static then
          bind x (← fresh { bal := 0, mode := .static }); return
    let agg ← fresh { bal := 1, mode := .owned, uniq := pu }
    if let some y := p then
      if isRc env y then
        let v ← spend "C-Ctor" y (allowNegative := true)
        let i ← infoOf v
        putInfo v { i with holder := some agg }
    if rc then bind x agg
  | .call p args =>
    let some callee := env.prog.find? p | fail "C-Call" s!"call of p{p}, absent from the dump"
    let mut lenders : List VId := []
    for (y, i) in args.zipIdx do
      if !isRc env y then continue
      if bit callee.borrowedMask i then
        lenders := (← requireLive "C-CallBorrow" y) :: lenders
      else discard <| spend "C-CallOwned" y
    if rc then
      if callee.retBorrowed then bind x (← fresh { bal := 0, mode := .borrowed lenders })
      else bind x (← fresh { bal := 1, mode := .owned, uniq := callee.retUnique })
  | .lowLevel op args uniqMask =>
    let some r := env.ops.get? op | fail "C-Op" s!"no RcEffect row for {op}"
    -- C-UniqueStamp: an independent check of rule (g); the real certifier
    -- reuses the solver's computeProcUniqueness here
    for (y, i) in args.zipIdx do
      if bit uniqMask i then
        if !(bit r.uniqCheck i) then fail "C-UniqueStamp" s!"{op}: unique stamp on argument {i}, which the op does not check"
        let v ← valOf y
        let iv ← infoOf v
        if !(iv.uniq && iv.bal == 1) then
          -- M-UniqueSeed: a parameter's uniqueness comes from the variant's
          -- `unique` seeds, which the dump does not print (G3), so the model
          -- gives up instead of rejecting
          if iv.param then
            fail "M-UniqueSeed" s!"{op}: argument {i} (l{y}, v{v}) is stamped unique and derives from a parameter; the variant's unique seeds are not in the dump"
          let why := if iv.mode == .static then " (static data)" else ""
          fail "C-UniqueStamp" s!"{op}: argument {i} (l{y}, v{v}) is stamped unique but is not born-unique with one unit (uniq={iv.uniq}, balance {iv.bal}){why}"
    -- C-BirthAlias: a result that is a consumed argument's own allocation
    -- (`result_aliases_consumed_args`, e.g. `list_map_prepare_reuse`) is
    -- unique exactly when that argument was born-unique with its one unit
    let mut aliasUniq := false
    for (y, i) in args.zipIdx do
      if bit r.aliasConsumed i && isRc env y then
        let iv ← infoOf (← valOf y)
        if iv.uniq && iv.bal == 1 then aliasUniq := true
    let mut lenders : List VId := []
    for (y, i) in args.zipIdx do
      if !isRc env y then continue
      -- retained positions: ARC emitted an explicit incref (or transfers the
      -- caller's unit), and the op moves that unit into its result
      if bit r.consume i || bit r.retain i then discard <| spend "C-OpConsume" y
      else if bit r.reprArgs i then pure ()
      else
        let v ← requireLive "C-OpRead" y
        if bit r.borrowsArgs i then lenders := v :: lenders
    if rc then
      if r.borrowsArgs != 0 then bind x (← fresh { bal := 0, mode := .borrowed lenders })
      else if r.retainResult then bind x (← fresh { bal := 0, mode := .borrowed lenders })
      else bind x (← fresh { bal := 1, mode := .owned, uniq := r.resultUnique || aliasUniq })
  | .unsupported t => fail "C-Unsupported" t

/-- Terminal balance check (C-Balance): every owned value is at zero. -/
def checkBalanced (where_ : String) : CM Unit := do
  let s ← get
  for (v, i) in s.info.toList do
    if i.mode == .owned && i.bal != 0 && !i.residual then
      let names := s.vals.toList.filterMap fun (x, w) => if w == v then some s!"l{x}" else none
      fail "C-Balance" s!"{where_}: value v{v} {names} ends with balance {i.bal} (slot={i.slot}, holder={i.holder}, taken={i.taken}, param={i.param})"

/-- Summary a join entry is keyed by: per bound local, its value's partition
    representative, balance and mode tag. -/
def summary (s : St) (only : Option (List Local) := none) : String := Id.run do
  -- values renamed by first appearance over the sorted locals, so two walks
  -- reaching a join with the same partition, balances, modes and taken slots
  -- agree regardless of how many fresh values earlier iterations created
  let rows := (s.vals.toList.mergeSort (fun a b => a.1 ≤ b.1)).filter fun (x, _) =>
    match only with | some ls => ls.contains x | none => true
  let mut names : Std.HashMap VId Nat := {}
  let mut out : Array String := #[]
  for (x, v) in rows do
    let i0 := (s.info.get? v).getD default
    if !liveV s v && i0.bal == 0 && !i0.residual then
      -- a dead name: any use in the body fails on every walk (C-Read or
      -- C-Bound alike), so it is the same entry as an unbound name
      continue
    let n := match names.get? v with
      | some n => n
      | none => names.size
    names := names.insert v n
    let i := (s.info.get? v).getD default
    let tag := match i.mode with | .owned => "o" | .borrowed _ => "b" | .static => "s"
    let live := if liveV s v then "L" else "D"
    out := out.push s!"{x}:{n}:{i.bal}:{tag}{live}:{i.taken}:{if i.residual then "r" else ""}"
  -- values no listed local names can only matter to the walk through the
  -- terminal balance check, which they fail on every walk or on none
  let named := names.toList.map (·.1)
  let outside := s.info.toList.any fun (v, i) =>
    !named.contains v && i.mode == .owned && i.bal != 0 && !i.residual
  if outside then out := out.push "outside-imbalance"
  return ",".intercalate out.toList

partial def certStmt (env : Env) (joins : Std.HashMap JoinId Stmt)
    (seen : Std.HashMap (JoinId × String) (List Local)) (s : Stmt) : StateT St (Except String) (Std.HashMap (JoinId × String) (List Local)) := do
  let note (t : String) : CM Unit := modify fun st => { st with trail := t :: st.trail }
  -- the model walks paths, where the real certifier merges at joins; past
  -- this budget it gives up (M-Budget), which is not a rejection
  modify fun st => { st with steps := st.steps + 1 }
  if (← get).steps > 200000 then fail "M-Budget" "walk budget exhausted (path-sensitive model; the real certifier merges at joins)"
  match s with
  | .assign x _ (.call p _) _ => note s!"l{x}=call p{p}"
  | .assign x _ (.ref k y tk) _ => note s!"l{x}={reprStr k |>.replace "RocArc.RefKind." ""} l{y}{if tk then " take" else ""}"
  | .assign x _ _ _ => note s!"l{x}=…"
  | .set x y _ => note s!"set l{x}:=l{y}"
  | .dec x _ => note s!"dec l{x}"
  | .inc x _ _ => note s!"inc l{x}"
  | .jump j => note s!"jump j{j}"
  | .ret x => note s!"ret l{x}"
  | _ => pure ()
  match s with
  | .assign x ty e k =>
    certExpr env x ty e
    certStmt env joins seen k
  | .set x y k =>
    if isRc env y then bind x (← valOf y)
    certStmt env joins seen k
  | .inc x n k =>
    if isRc env x then
      let v ← requireLive "C-Inc" x
      let i ← infoOf v
      if i.mode != .static then putInfo v { i with bal := i.bal + n, uniq := false }
    certStmt env joins seen k
  | .dec x k | .free x k =>
    if isRc env x then discard <| spend "C-Dec" x
    certStmt env joins seen k
  | .decIfInit _ _ x k =>
    if isRc env x && (← get).vals.contains x then discard <| spend "C-Dec" x
    certStmt env joins seen k
  | .initUninit x k =>
    modify fun st => { st with vals := st.vals.erase x }
    certStmt env joins seen k
  | .use x k =>
    if isRc env x then discard <| requireLive "C-Read" x
    certStmt env joins seen k
  | .switch _ cases d cont =>
    if cont.isSome then fail "C-Unsupported" "switch continuation"
    let st ← get
    let mut seen := seen
    for (_, b) in cases ++ [(0, d)] do
      let (seen', _) ← (certStmt env joins seen b).run st
      seen := seen'
    return seen
  | .switchInit _ _ p a b =>
    let st ← get
    -- α gap G4: the condition bits are not tracked; as in the machine,
    -- initializedness is read from the payload itself, so a payload no path
    -- here bound takes only the uninitialized branch
    let (seen1, _) ← if st.vals.contains p || !(isRc env p) then (certStmt env joins seen a).run st
      else pure (seen, st)
    let st2 := { st with vals := st.vals.erase p }
    let (seen2, _) ← (certStmt env joins seen1 b).run st2
    return seen2
  | .join j _ rem body => certStmt env (joins.insert j body) seen rem
  | .jump j =>
    let some body := joins.get? j | fail "C-Jump" s!"jump to unknown j{j}"
    let key := (j, summary (← get) (env.liveIn.get? j))
    -- uniqueness is a meet, not part of the key (design.md: born-unique facts
    -- merge by ∧ at a join): an entry at least as unique as a walked one is
    -- covered; otherwise walk once more under the meet
    let st ← get
    let ls := (env.liveIn.get? j).getD []
    let uq := ls.filter fun l => match st.vals.get? l with
      | some v => ((st.info.get? v).map (·.uniq)).getD false
      | none => false
    let meet := match seen.get? key with
      | some old => old.filter uq.contains
      | none => uq
    if let some old := seen.get? key then
      if old.all uq.contains then return seen
    for l in ls do
      if !meet.contains l then
        if let some v := st.vals.get? l then
          if let some i := st.info.get? v then
            modify fun st' => { st' with info := st'.info.insert v { i with uniq := false } }
    let mine := (seen.toList.filter (·.1.1 == j)).map (·.1.2)
    if mine.length > 2000 then
      -- which locals vary across the entries: per local, its distinct rows
      let rows := mine.flatMap fun e => (e.splitOn ",").filter (· ≠ "")
      let locs := (rows.map fun r => (r.splitOn ":").head!).eraseDups
      let var := locs.filterMap fun l =>
        let vs := (rows.filter fun r => (r.splitOn ":").head! == l).eraseDups
        let absent := mine.any fun e => !((e.splitOn ",").any fun r => (r.splitOn ":").head! == l)
        if vs.length > 1 || absent then some s!"l{l}{if absent then "(sometimes absent)" else ""}:{vs.take 4}" else none
      fail "M-Growth" s!"join j{j}: {mine.length} distinct entry states; varying: {(var.take 8)}"
    certStmt env joins (seen.insert key meet) body
  | .ret x =>
    if isRc env x then
      if env.proc.retBorrowed then discard <| requireLive "C-RetBorrowed" x
      else discard <| spend "C-Ret" x
    checkBalanced s!"ret l{x}"
    return seen
  | .crash msg =>
    if let some m := msg then
      if isRc env m then discard <| spend "C-Crash" m
    checkBalanced "crash"; return seen
  | .runtimeError => checkBalanced "runtime_error"; return seen
  | .unsupported t => fail "C-Unsupported" t

def typesOf (p : Proc) : Std.HashMap Local Ty := Id.run do
  let mut m : Std.HashMap Local Ty := {}
  for (x, t) in p.params do m := m.insert x t
  let rec go (s : Stmt) (m : Std.HashMap Local Ty) (fuel : Nat) : Std.HashMap Local Ty :=
    match fuel with
    | 0 => m
    | fuel + 1 =>
      match s with
      | .assign x t _ k => go k (m.insert x t) fuel
      | .set x y k =>
        let m := if m.contains x then m else match m.get? y with
          | some t => m.insert x t
          | none => m
        go k m fuel
      | .inc _ _ k | .dec _ k | .free _ k | .decIfInit _ _ _ k
      | .initUninit _ k | .use _ k => go k m fuel
      | .switch _ cs d c =>
        let m := cs.foldl (fun m (_, b) => go b m fuel) m
        let m := go d m fuel
        match c with | some c => go c m fuel | none => m
      | .switchInit _ _ _ a b => go b (go a m fuel) fuel
      | .join _ _ r b => go b (go r m fuel) fuel
      | _ => m
  -- twice, so a join param set from a local typed later in print order is typed
  return go p.body (go p.body m 100000) 100000

/-- Every local a statement mentions (an over-approximation of what it reads). -/
partial def _root_.RocArc.Stmt.mentions : Stmt → List Local
  | .assign x _ e k =>
    let es := match e with
      | .ref _ y _ => [y] | .call _ as | .lowLevel _ as _ | .list as | .struct_ as => as
      | .tag _ _ (some y) => [y] | _ => []
    x :: es ++ k.mentions
  | .set x y k => x :: y :: k.mentions
  | .inc x _ k | .dec x k | .free x k | .initUninit x k | .use x k => x :: k.mentions
  | .decIfInit c _ x k => c :: x :: k.mentions
  | .switch x cs d c => x :: (cs.flatMap (·.2.mentions)) ++ d.mentions ++ (match c with | some c => c.mentions | none => [])
  | .switchInit c _ p a b => c :: p :: a.mentions ++ b.mentions
  | .join _ ps r b => ps ++ r.mentions ++ b.mentions
  | .ret x => [x]
  | .crash (some m) => [m]
  | _ => []

partial def _root_.RocArc.Stmt.jumps : Stmt → List JoinId
  | .assign _ _ _ k | .set _ _ k | .inc _ _ k | .dec _ k | .free _ k | .initUninit _ k
  | .use _ k | .decIfInit _ _ _ k => k.jumps
  | .switch _ cs d c => cs.flatMap (·.2.jumps) ++ d.jumps ++ (match c with | some c => c.jumps | none => [])
  | .switchInit _ _ _ a b => a.jumps ++ b.jumps
  | .join _ _ r b => r.jumps ++ b.jumps
  | .jump j => [j]
  | _ => []

partial def _root_.RocArc.Stmt.joinBodies : Stmt → List (JoinId × Stmt)
  | .assign _ _ _ k | .set _ _ k | .inc _ _ k | .dec _ k | .free _ k | .initUninit _ k
  | .use _ k | .decIfInit _ _ _ k => k.joinBodies
  | .switch _ cs d c => cs.flatMap (·.2.joinBodies) ++ d.joinBodies ++ (match c with | some c => c.joinBodies | none => [])
  | .switchInit _ _ _ a b => a.joinBodies ++ b.joinBodies
  | .join j _ r b => (j, b) :: r.joinBodies ++ b.joinBodies
  | _ => []

/-- Per join, the locals its body, or any join body it can reach by jumping,
    mentions. A walk of the body depends on the entry state only through
    these locals (and the terminal balance check; see `summary`). -/
def joinLiveIn (body : Stmt) : Std.HashMap JoinId (List Local) := Id.run do
  let bodies := body.joinBodies
  let direct : Std.HashMap JoinId (List Local × List JoinId) :=
    bodies.foldl (fun m (j, b) => m.insert j (b.mentions.eraseDups, b.jumps.eraseDups)) {}
  let mut out : Std.HashMap JoinId (List Local) := {}
  for (j, _) in bodies do
    let mut seen : List JoinId := [j]
    let mut todo : List JoinId := [j]
    let mut acc : List Local := []
    let mut fuel := bodies.length + 1
    while fuel > 0 && !todo.isEmpty do
      fuel := fuel - 1
      let cur := todo
      todo := []
      for k in cur do
        let (ms, js) := (direct.get? k).getD ([], [])
        acc := acc ++ ms
        for k' in js do
          if !seen.contains k' then
            seen := k' :: seen
            todo := k' :: todo
    out := out.insert j acc.eraseDups
  return out

end Cert

/-- Σ ⊢ p : the certifier judgment for one procedure. -/
def certifyProc (P : Program) (rcLayouts : List Nat) (ops : Std.HashMap String RcEffect) (p : Proc) : Except String Unit := do
  let types := Cert.typesOf p
  -- join params are typed by their first assignment; record them too
  let env : Cert.Env := { prog := P, proc := p, rcLayouts, ops, types, liveIn := Cert.joinLiveIn p.body }
  let init : Cert.CM Unit := do
    for ((x, t), i) in p.params.zipIdx do
      if t.mayBeRc rcLayouts then
        let mode := if bit p.borrowedMask i then Cert.Mode.borrowed [] else .owned
        let bal : Int := if bit p.borrowedMask i then 0 else 1
        -- α gap: an erased callable parameter's ownership follows the proc's
        -- erased-call plan (arc_certify `certifyErasedCallArgsPlan`), which the
        -- dump does not print; its balance is not checked
        let residual := t.kind == .erased
        Cert.bind x (← Cert.fresh { bal, mode, residual, param := true })
  let prog : Cert.CM Unit := do
    init
    discard <| Cert.certStmt env {} {} p.body
  match (prog.run {}) with
  | .ok _ => .ok ()
  | .error e => .error e

end RocArc

namespace RocArc
/-- Layout ids that may hold a refcounted allocation: those an RC helper names,
    closed under "an aggregate built from a refcounted operand is refcounted"
    and under field reads of refcounted values out of an aggregate. -/
def inferRcLayouts (P : Program) (seed : List Nat) : List Nat := Id.run do
  let types : Std.HashMap Local Ty := P.procs.foldl (fun m p => (Cert.typesOf p).fold (fun m k v => m.insert k v) m) {}
  let rec collect (s : Stmt) (acc : List (Ty × List Local)) (fuel : Nat) : List (Ty × List Local) :=
    match fuel with
    | 0 => acc
    | fuel + 1 =>
      match s with
      | .assign _ t (.struct_ xs) k | .assign _ t (.list xs) k => collect k ((t, xs) :: acc) fuel
      | .assign _ t (.tag _ _ (some p)) k => collect k ((t, [p]) :: acc) fuel
      -- a projection: the container is refcounted if the part read out is
      | .assign x _ (.ref (.field _) y _) k | .assign x _ (.ref (.tagPayload _ _) y _) k
      | .assign x _ (.ref (.tagPayloadStruct _) y _) k =>
        collect k (((types.get? y).getD default, [x]) :: acc) fuel
      | .assign _ _ _ k | .set _ _ k | .inc _ _ k | .dec _ k | .free _ k | .decIfInit _ _ _ k
      | .initUninit _ k | .use _ k => collect k acc fuel
      | .switch _ cs d c =>
        let acc := cs.foldl (fun a (_, b) => collect b a fuel) acc
        let acc := collect d acc fuel
        match c with | some c => collect c acc fuel | none => acc
      | .switchInit _ _ _ a b => collect b (collect a acc fuel) fuel
      | .join _ _ r b => collect b (collect r acc fuel) fuel
      | _ => acc
  let ctors := P.procs.foldl (fun acc p => collect p.body acc 100000) []
  let mut rc := seed
  let mut changed := true
  while changed do
    changed := false
    for (t, xs) in ctors do
      match t.idx with
      | some i =>
        if !rc.contains i && xs.any (fun x => match types.get? x with
            | some tx => tx.mayBeRc rc | none => false) then
          rc := i :: rc; changed := true
      | none => pure ()
  return rc
end RocArc
