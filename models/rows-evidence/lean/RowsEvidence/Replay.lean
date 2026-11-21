import RowsEvidence.Check
/-!
  Monotype replay at closed root substitutions, and the four decidable checks.

  For every reachable request `(template, closed type)` the template's checked type is copied into
  a fresh instantiation graph (`copyAll`), unified with the request in `Mode.mono` (where a closed
  row gaining a label is a compiler bug, solve.zig `unifyTagRows`), the checker's subsumption relations are
  replayed, every call edge yields a new request after sealing (M-SealDefault: every unresolved
  variable seals to `[]`/`{}`, solve.zig `materializeUnresolved`), and every dispatch site and evidence entry is
  checked against the sealed types.

  (a) instantiation monotonicity: `Viol.widen`, `Viol.notSubsumed`
  (b) evidence completeness/functionality: `Viol.missingEvidence`, `Viol.duplicateEvidence`
  (c) unreachable-dispatch soundness: `Viol.unreachableInhabited`
  (d) codec contract completeness: `codecCheck` (over shapes, below)
-/
namespace RE

/-- Rule toggles. The defaults are the current rules; the others restore a pre-fix rule
    so the checks can be calibrated against known, fixed bugs (Calibrate.lean). -/
structure Cfg where
  /-- M-ClosedResultWiden (lower.zig `checkedResultRowIsClosed`, `widenClosedResultRowToRequest`): a closed DIRECT result row or `Try` error row
      of a template widens to the request's row -/
  boundaryWiden : Bool := true
  /-- pre-#12625: a construction forwarding a parameter (`Err(tag)`) was lowered at the
      composed result row's cell, i.e. its payload related EXACTLY to the composed row -/
  exactConstructionPayload : Bool := false
  /-- pre-#12552: a point-free alias did not compose its target's evidence; each of the
      alias's evidence entries published as `unreachable` -/
  aliasDropsEvidence : Bool := false
  deriving Repr, Inhabited

inductive Viol where
  | widen (tmpl : String) (why : String)
  | notSubsumed (tmpl : String) (kind : RelKind) (detail : String)
  | missingEvidence (tmpl : String) (meth : String) (ty : String)
  | duplicateEvidence (tmpl : String) (meth : String)
  | unreachableInhabited (tmpl : String) (meth : String) (ty : String)
  deriving Repr, Inhabited

def Viol.show : Viol → String
  | .widen t w => s!"(a) replay of {t}: {w}"
  | .notSubsumed t k d => s!"(a) {repr k} in {t}: {d}"
  | .missingEvidence t m ty => s!"(b) {t}: no evidence for {m} at {ty}"
  | .duplicateEvidence t m => s!"(b) {t}: two evidence entries for {m}"
  | .unreachableInhabited t m ty => s!"(c) {t}: dispatch {m} published unreachable, reached at inhabited {ty}"

/-- Copy everything reachable (a fresh per-request graph). -/
partial def copyAll (n : Nat) (m : List (Nat × Nat)) : UM (Nat × List (Nat × Nat)) := do
  let r ← find n
  if let some (_, c) := m.find? (·.1 == r) then return (c, m)
  let v ← freshFlex
  let mut m := (r, v) :: m
  let c ← content r
  match c with
  | .flex mk => do
      let mut reqs := []
      for q in mk.reqs do
        let (s', m') ← copyAll q.sig m; m := m'
        reqs := reqs ++ [{ q with sig := s' }]
      setContent v (.flex { mk with reqs, bounded := false })
  | .rigid _ mk => setContent v (.flex { mk with reqs := [], bounded := false })
  | .struct ft mk => do
      let ft' ← match ft with
        | .fn as ret => do
            let mut as' := []
            for a in as do let (a', m') ← copyAll a m; m := m'; as' := as' ++ [a']
            let (r', m') ← copyAll ret m; m := m'
            pure (FT.fn as' r')
        | .record fs e => do
            let mut fs' := []
            for (f, a) in fs do let (a', m') ← copyAll a m; m := m'; fs' := fs' ++ [(f, a')]
            let (e', m') ← copyAll e m; m := m'
            pure (FT.record fs' e')
        | .tags ts e => do
            let mut ts' := []
            for (t, as) in ts do
              let mut as' := []
              for a in as do let (a', m') ← copyAll a m; m := m'; as' := as' ++ [a']
              ts' := ts' ++ [(t, as')]
            let (e', m') ← copyAll e m; m := m'
            pure (FT.tags ts' e')
        | .nom nm as => do
            let mut as' := []
            for a in as do let (a', m') ← copyAll a m; m := m'; as' := as' ++ [a']
            pure (FT.nom nm as')
        | .list e => do let (e', m') ← copyAll e m; m := m'; pure (FT.list e')
        | other => pure other
      -- an uncommitted [] publishes as a plain [] (sealed_type_view.zig `payload`)
      setContent v (.struct ft' { mk with uncommitted := false, bounded := false })
  | .link _ => pure ()
  return (v, m)

/-- M-SealDefault: every unresolved variable seals to `[]` (`{}` in a record tail). -/
partial def sealTy (n : Nat) (inRecTail : Bool := false) (seen : List Nat := []) : UM (List Nat) := do
  let r ← find n
  if seen.contains r then return seen
  let seen := r :: seen
  match (← content r) with
  | .flex _ | .rigid _ _ => do
      setContent r (.struct (if inRecTail then .emptyRec else .emptyTags) {})
      return seen
  | .struct ft _ =>
    match ft with
    | .fn as ret => do
        let mut s := seen
        for a in as do s ← sealTy a false s
        sealTy ret false s
    | .record fs e => do
        let mut s := seen
        for (_, a) in fs do s ← sealTy a false s
        sealTy e true s
    | .tags ts e => do
        let mut s := seen
        for (_, as) in ts do for a in as do s ← sealTy a false s
        sealTy e false s
    | .nom _ as => do
        let mut s := seen
        for a in as do s ← sealTy a false s
        return s
    | .list e => sealTy e false seen
    | _ => return seen
  | _ => return seen

/-- Inhabited after sealing: anything but an empty tag union (payloads checked recursively). -/
partial def inhabited (n : Nat) (fuel : Nat := 30) : UM Bool := do
  if fuel == 0 then return true
  let r ← find n
  match (← content r) with
  | .struct .emptyTags _ => return false
  | .struct (.tags _ _) _ => do
      let (ts, _) ← gatherTags r
      for (_, ps) in ts do
        if (← ps.allM (inhabited · (fuel - 1))) then return true
      return false
  | _ => return true

/-- The template's direct result row or `Try` error row, when closed (lower.zig `checkedResultRowIsClosed`). -/
def closedResultRowTail (fnTy : Nat) : UM (Option Nat) := do
  match (← content fnTy) with
  | .struct (.fn _ ret) _ =>
      let row ← match (← content ret) with
        | .struct (.nom "Try" [_, e]) _ => pure (some e)
        | .struct (.tags _ _) _ => pure (some ret)
        | _ => pure none
      match row with
      | some rw => do
          let (_, tail) ← gatherTags rw
          match (← content tail) with
          | .struct .emptyTags _ => return some tail
          | _ => return none
      | none => return none
  | _ => return none

/-! Publication-time classification (checked_artifact.zig `resolveObligationStep`) -/

inductive Res where
  | direct (target : String)
  | forward
  | unreachable
  | error
  deriving Repr, BEq, Inhabited

def classifyAt (v : Nat) (m : String) : UM Res := do
  let r ← find v
  match (← content r) with
  | .struct ft _ =>
      match ← methodType ft m with
      | some _ => return .direct (match ft with | .nom n _ => s!"{n}.{m}" | .u64 => s!"U64.{m}" | _ => s!"Str.{m}")
      | none => return .error
  | .flex mk =>
      -- E-Unpinned (static_dispatch_registry.zig `unpinnedDispatchResolution`): a still-flex dispatcher nothing pins is `unreachable`;
      -- a quantified one with the requirement forwards (evidence_dependent)
      if (← levelOf r) == genericLevel && mk.reqs.any (·.meth == m) then return .forward
      else return .unreachable
  | .rigid _ _ => return .forward
  | _ => return .error

structure EvEntry where
  edge : Nat
  gv   : Nat
  meth : String
  res  : Res
  deriving Repr, Inhabited

/-- Evidence vectors: one entry per (edge, quantified var, method), in instantiation order. -/
def evidence (ck : Checked) (cfg : Cfg) : UM (List EvEntry) := do
  let mut out := []
  let aliasNames := ck.defs.filterMap fun d => match d.body with | .var _ => some d.name | _ => none
  for i in [0:ck.cs.edges.size] do
    let e := ck.cs.edges[i]!
    for (gv, iv) in e.pairs do
      let mk := marksOf (← content gv)
      let mut seen : List String := []
      for q in mk.reqs do
        if seen.contains q.meth then continue
        seen := q.meth :: seen
        let res ← if cfg.aliasDropsEvidence && aliasNames.contains e.tmpl then pure Res.unreachable
                  else classifyAt iv q.meth
        out := out ++ [{ edge := i, gv, meth := q.meth, res }]
  return out

/-! Replay -/

structure RState where
  done  : List String := []
  viols : List Viol := []

def tryU {α} (m : UM α) : UM (Except UErr α) := do
  let s ← get
  match m.run s with
  | .ok (a, s') => set s'; return .ok a
  | .error e => return .error e

/-- Replay one request; returns the new requests (callee, sealed instance var). -/
def replayOne (ck : Checked) (cfg : Cfg) (ev : List EvEntry) (tmpl : String) (root : Nat) (req : Option Nat)
    : StateT RState UM (List (String × Nat)) := do
  let viol (v : Viol) : StateT RState UM Unit := modify fun s => { s with viols := s.viols ++ [v] }
  let (fc, m0) ← (copyAll root [] : UM _)
  let mut m := m0
  if let some rq := req then
    if cfg.boundaryWiden then
      if let some tail ← (closedResultRowTail fc : UM _) then
        (setContent tail (.flex {}) : UM _)
    match ← (tryU (unify .mono fc rq) : UM _) with
    | .error e => viol (.widen tmpl e.show)
    | .ok _ => pure ()
  -- the checker's subsumption relations
  for rel in ck.cs.rels.toList.filter (·.tmpl == tmpl) do
    let (s, m1) ← (copyAll rel.src m : UM _); m := m1
    let (d, m2) ← (copyAll rel.dst m : UM _); m := m2
    if cfg.exactConstructionPayload && rel.kind == .constructionPayload then
      match ← (tryU (unify .mono s d) : UM _) with
      | .error e => viol (.notSubsumed tmpl rel.kind e.show)
      | .ok _ => pure ()
  -- sealTy the template graph
  discard <| (sealTy fc : UM _)
  for rel in ck.cs.rels.toList.filter (·.tmpl == tmpl) do
    -- monotonicity of every widening: the source's labels are all in the destination
    let (s, m1) ← (copyAll rel.src m : UM _); m := m1
    let (d, m2) ← (copyAll rel.dst m : UM _); m := m2
    discard <| (sealTy s : UM _); discard <| (sealTy d : UM _)
    let (st, _) ← (gatherTags s : UM _)
    let (dt, _) ← (gatherTags d : UM _)
    for (l, _) in st do
      if !(dt.any (·.1 == l)) then viol (.notSubsumed tmpl rel.kind s!"label {l} missing from destination")
  -- dispatch sites at the sealed types (b)
  for ds in ck.cs.dsites.toList.filter (·.tmpl == tmpl) do
    let (rv, m1) ← (copyAll ds.recv m : UM _); m := m1
    discard <| (sealTy rv : UM _)
    let r ← (classifyAt rv ds.meth : UM _)
    if r == .error || r == .unreachable then
      if ← (inhabited rv : UM _) then viol (.missingEvidence tmpl ds.meth (← (showTy rv : UM _)))
  -- evidence entries of this template's edges (b, c)
  let mut newReqs := []
  for i in [0:ck.cs.edges.size] do
    let e := ck.cs.edges[i]!
    if e.tmpl != tmpl then continue
    let (iv, m1) ← (copyAll e.inst m : UM _); m := m1
    discard <| (sealTy iv : UM _)
    newReqs := newReqs ++ [(e.callee, iv)]
    let entries := ev.filter (·.edge == i)
    for en in entries do
      if (entries.filter fun x => x.gv == en.gv && x.meth == en.meth).length > 1 then
        viol (.duplicateEvidence tmpl en.meth)
      let (sv, m2) ← (copyAll (e.pairs.find? (·.1 == en.gv) |>.map (·.2) |>.getD en.gv) m : UM _); m := m2
      discard <| (sealTy sv : UM _)
      match en.res with
      | .unreachable =>
          if ← (inhabited sv : UM _) then viol (.unreachableInhabited tmpl en.meth (← (showTy sv : UM _)))
      | .error => viol (.missingEvidence tmpl en.meth "error")
      | _ => pure ()
  return newReqs

partial def replayProgram (ck : Checked) (cfg : Cfg := {}) : List Viol := Id.run do
  let st := ck.cs.st
  let act : StateT RState UM Unit := do
    let ev ← (evidence ck cfg : UM _)
    let rootOf (x : String) : Option Nat :=
      (ck.cs.schemes.find? (·.1 == x)).map (·.2) <|> (ck.cs.mono.find? (·.1 == x)).map (·.2)
    let mut work : List (String × Option Nat) := ck.roots.map fun (n, _) => (n, none)
    let mut fuel := 200
    while fuel > 0 do
      fuel := fuel - 1
      match work with
      | [] => break
      | (t, rq) :: rest =>
        work := rest
        let key ← match rq with
          | some v => do pure s!"{t}@{← (showTy v : UM _)}"
          | none => pure t
        if (← get).done.contains key then continue
        modify fun s => { s with done := key :: s.done }
        let root := if t.startsWith "expect#" then
            (ck.roots.find? (·.1 == t)).map (·.2) |>.getD 0
          else (rootOf t).getD 0
        if !t.startsWith "expect#" && (rootOf t).isNone then continue
        let news ← replayOne ck cfg ev t root rq
        work := work ++ news.map fun (c, v) => (c, some v)
  match (act.run {}).run st with
  | .ok ((_, rs), _) => rs.viols
  | .error e => [.widen "replay" e.show]

/-! (d) Generated codec contracts over shapes: `calls(K) = Req(T, F)` -/

/-- `Req(T, F)` for the derived encoder of a JSON-like format F: the multiset of format methods
    a generated body for shape T calls (Check.zig `encodeFormatMethodName`, `validateDerivedEncodeRecordMethods`). -/
partial def codecReq : G → List String
  | .u64 => ["encode_u64"]
  | .str => ["encode_str"]
  | .tags ts => "encode_tag" :: ts.flatMap (fun (_, ps) => ps.flatMap codecReq)
  | .record fs => "encode_record" :: (if fs.isEmpty then [] else ["rename_field"]) ++ fs.flatMap (codecReq ·.2)
  | .list t => "encode_list" :: codecReq t
  | .try_ ok err => "encode_tag" :: codecReq ok ++ codecReq err
  | _ => []

/-- The checker's recorded calls K. `preFix12229` drops the list children of a tag payload
    inside a record field (the shape of #12229: `parse_list_start` missing). -/
partial def codecRecorded (preFix12229 : Bool) (inRecord : Bool := false) (inTag : Bool := false) : G → List String
  | .u64 => ["encode_u64"]
  | .str => ["encode_str"]
  | .tags ts => "encode_tag" :: ts.flatMap (fun (_, ps) => ps.flatMap (codecRecorded preFix12229 inRecord true))
  | .record fs => "encode_record" :: (if fs.isEmpty then [] else ["rename_field"]) ++ fs.flatMap (codecRecorded preFix12229 true inTag ·.2)
  | .list t => if preFix12229 && inRecord && inTag then [] else "encode_list" :: codecRecorded preFix12229 inRecord inTag t
  | .try_ ok err => "encode_tag" :: codecRecorded preFix12229 inRecord true ok ++ codecRecorded preFix12229 inRecord true err
  | _ => []

def msetEq (a b : List String) : Bool :=
  (a ++ b).all fun x => a.count x == b.count x

/-- (d) over every shape up to the given depth. Returns the shapes where K ≠ Req. -/
partial def shapes : Nat → List G
  | 0 => [.u64, .str]
  | d + 1 =>
    let sub := shapes d
    [.u64, .str] ++ sub.map (fun t => G.list t) ++ sub.map (fun t => G.tags [("A", [t]), ("B", [])])
      ++ sub.map (fun t => G.record [("f", t)]) ++ sub.map (fun t => G.try_ t (.tags [("E", [])]))

def codecCheck (preFix12229 : Bool) (depth : Nat := 3) : List G :=
  (shapes depth).filter fun t => !msetEq (codecRecorded preFix12229 false false t) (codecReq t)

end RE
