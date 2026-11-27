import RowsEvidence.Types
import RowsEvidence.Syntax
/-!
  The modeled checker: HM inference with rows, encoding the rules as the Zig source implements
  them (see RULES.md for the rule table and citations). Besides a solved store it publishes the
  data post-check stages replay:

  * `Edge`: one generic instantiation (template, callee, scheme var → instance var pairs);
  * `Rel`: a relation the checker made by *subsumption* instead of unification (a `?` projection,
    a forwarded construction payload, a result-position row coercion). Monotype must replay these
    as widenings, never as exact unifications;
  * `DSite`: a static-dispatch site, classified at publication.
-/
namespace RE

structure Edge where
  tmpl   : String
  callee : String
  inst   : Nat
  /-- scheme generic var ↦ instance var, in instantiation (canonical) order -/
  pairs  : List (Nat × Nat)
  /-- the use is inside the callee's own recursive group (no instantiation recorded) -/
  inGroup : Bool := false
  deriving Repr, Inhabited

inductive RelKind where
  /-- R-TryProject: a closed `?` error row's heads copied onto a fresh tail -/
  | tryProject
  /-- R-TryTail: a tagless contribution (e.g. `Err(param)`) is the composed row's tail;
      the construction's payload slot is NOT the composed row -/
  | constructionPayload
  /-- R-Coerce: a value flowing into an annotated result through a re-opened copy -/
  | resultCoerce
  deriving Repr, BEq, Inhabited

structure Rel where
  tmpl : String
  kind : RelKind
  src  : Nat
  dst  : Nat
  deriving Repr, Inhabited

structure DSite where
  tmpl : String
  recv : Nat
  meth : String
  deriving Repr, Inhabited

structure CState where
  st      : Store
  schemes : List (String × Nat) := []
  /-- names whose type is monomorphic (consts, and members of the group being checked) -/
  mono    : List (String × Nat) := []
  edges   : Array Edge := #[]
  rels    : Array Rel := #[]
  dsites  : Array DSite := #[]
  /-- `?` contributions of the lambdas being checked, innermost first -/
  frames  : List (Array Nat) := []
  tmpl    : String := ""
  /-- group members currently being checked (in-group uses are not instantiated) -/
  group   : List String := []
  deriving Inhabited

abbrev CM := StateT CState (Except String)

def liftU {α} (m : UM α) : CM α := do
  let s ← get
  match m.run s.st with
  | .ok (a, st') => set { s with st := st' }; return a
  | .error e => throw e.show

def cfresh (c : Content) : CM Nat := liftU (fresh c)
def cstruct (ft : FT) : CM Nat := liftU (mkStruct ft)
def cflex : CM Nat := liftU freshFlex
def cunify (a b : Nat) : CM Unit := liftU (unify .checker a b)

def builtinNoms : List NomDecl := [
  { name := "Try", arity := 2, backing := .tags [("Ok", [.param 0]), ("Err", [.param 1])] },
  { name := "Nm", arity := 0, backing := .tags [("Nm", [.u64])],
    methods := [("to_str", 1), ("bump", 1), ("label", 1)] },
  { name := "Box", arity := 1, backing := .tags [("Box", [.param 0])], methods := [("unbox", 1)] }
]

/-- Instantiate a method's type for a receiver of known nominal/primitive head. -/
def methodType (recvHead : FT) (m : String) : UM (Option Nat) := do
  let s ← mkStruct .str
  match recvHead, m with
  | .u64, "to_str" => do let u ← mkStruct .u64; return some (← mkStruct (.fn [u] s))
  | .str, "concat" => do let a ← mkStruct .str; let b ← mkStruct .str; return some (← mkStruct (.fn [a, b] s))
  | .nom "Nm" [], "to_str" | .nom "Nm" [], "label" => do
      let n ← mkStruct (.nom "Nm" []); return some (← mkStruct (.fn [n] s))
  | .nom "Nm" [], "bump" => do
      let n ← mkStruct (.nom "Nm" []); let n2 ← mkStruct (.nom "Nm" []); return some (← mkStruct (.fn [n] n2))
  | .nom "Box" [_], "unbox" => do
      let a ← freshFlex; let b ← mkStruct (.nom "Box" [a]); return some (← mkStruct (.fn [b] a))
  | _, _ => return none

/-! Generalization and instantiation (generalize.zig; R-Gen, R-InstDropsBound) -/

partial def children (n : Nat) : UM (List Nat) := do
  let r ← find n
  let c ← content r
  let reqSigs := (marksOf c).reqs.map (·.sig)
  match c with
  | .struct ft _ => match ft with
    | .fn as r' => return as ++ [r'] ++ reqSigs
    | .record fs e => return fs.map (·.2) ++ [e] ++ reqSigs
    | .tags ts e => return (ts.map (·.2)).flatten ++ [e] ++ reqSigs
    | .nom _ as => return as ++ reqSigs
    | .list e => return [e] ++ reqSigs
    | _ => return reqSigs
  | _ => return reqSigs

partial def reachable (n : Nat) (seen : List Nat := []) : UM (List Nat) := do
  let r ← find n
  if seen.contains r then return seen
  let mut acc := r :: seen
  for ch in (← children r) do acc ← reachable ch acc
  return acc

def generalize (root : Nat) (outer : Nat) : UM Unit := do
  for v in (← reachable root) do
    if (← levelOf v) > outer then setLevel v genericLevel

def pinLevel (root : Nat) (l : Nat) : UM Unit := do
  for v in (← reachable root) do setLevel v l

partial def copyGeneric (n : Nat) (m : List (Nat × Nat)) : UM (Nat × List (Nat × Nat)) := do
  let r ← find n
  if let some (_, c) := m.find? (·.1 == r) then return (c, m)
  if (← levelOf r) != genericLevel then return (r, m)
  let c ← content r
  let mk := fun (cnt : Content) => fresh cnt
  match c with
  | .flex mk0 | .rigid _ mk0 => do
      let v ← mk (.flex {})
      let mut m := (r, v) :: m
      let mut reqs := []
      for q in mk0.reqs do
        let (s', m') ← copyGeneric q.sig m
        m := m'
        reqs := reqs ++ [{ q with sig := s' }]
      -- the bound is not copied by instantiation (the `bounded_row_ext` descriptor flag, types.zig)
      setContent v (.flex { reqs, uncommitted := false, bounded := false })
      return (v, m)
  | .struct ft mk0 => do
      let v ← mk (.flex {})
      let mut m := (r, v) :: m
      let cp := fun (x : Nat) (m : List (Nat × Nat)) => copyGeneric x m
      let ft' ← match ft with
        | .fn as ret => do
            let mut as' := []
            for a in as do let (a', m') ← cp a m; m := m'; as' := as' ++ [a']
            let (ret', m') ← cp ret m; m := m'
            pure (FT.fn as' ret')
        | .record fs e => do
            let mut fs' := []
            for (f, a) in fs do let (a', m') ← cp a m; m := m'; fs' := fs' ++ [(f, a')]
            let (e', m') ← cp e m; m := m'
            pure (FT.record fs' e')
        | .tags ts e => do
            let mut ts' := []
            for (t, as) in ts do
              let mut as' := []
              for a in as do let (a', m') ← cp a m; m := m'; as' := as' ++ [a']
              ts' := ts' ++ [(t, as')]
            let (e', m') ← cp e m; m := m'
            pure (FT.tags ts' e')
        | .nom nm as => do
            let mut as' := []
            for a in as do let (a', m') ← cp a m; m := m'; as' := as' ++ [a']
            pure (FT.nom nm as')
        | .list e => do let (e', m') ← cp e m; m := m'; pure (FT.list e')
        | other => pure other
      setContent v (.struct ft' { mk0 with bounded := false })
      return (v, m)
  | .link _ => return (r, m)

/-! Annotations (R-Polarity: output rows implicitly open with a bounded tail, input rows closed) -/

partial def annTy (g : G) (output : Bool) (mode : Nat) : CM Nat := do
  match g with
  | .fn as r => do
      let mut vs := []
      for a in as do vs := vs ++ [← annTy a false 0]
      let rv ← annTy r true mode
      cstruct (.fn vs rv)
  | .u64 => cstruct .u64
  | .str => cstruct .str
  | .nm => cstruct (.nom "Nm" [])
  | .box t => do let a ← annTy t output 0; cstruct (.nom "Box" [a])
  | .list t => do let a ← annTy t output 0; cstruct (.list a)
  | .record fs => do
      let mut out := []
      for (f, t) in fs do out := out ++ [(f, ← annTy t output 0)]
      let e ← cstruct .emptyRec
      cstruct (.record out e)
  | .tags ts => do
      let mut out := []
      for (c, ps) in ts do
        let mut vs := []
        for p in ps do vs := vs ++ [← annTy p output 0]
        out := out ++ [(c, vs)]
      let tail ← if mode == 2 then cfresh (.rigid "#others" {})
        else if output then cfresh (.flex { bounded := true })
        else cstruct .emptyTags
      cstruct (.tags out tail)
  | .try_ ok err => do
      let o ← annTy ok output 0
      let e ← if mode == 1 then cflex else annTy err output mode
      cstruct (.nom "Try" [o, e])

/-! The result-position coercion (design.md "Row Subsumption"; `Check.widenedClosedRowsCopy`).
    Reach: the root row, tag payloads of reached rows, nominal backings (Try, Box). -/

partial def widenCopy (n : Nat) : UM (Nat × Bool) := do
  let r ← find n
  match (← content r) with
  | .struct (.tags _ _) _ => do
      let (ts, tail) ← gatherTags r
      let closed ← match (← content tail) with
        | .struct .emptyTags _ => pure true
        | .flex m => pure m.bounded
        | _ => pure false
      let mut changed := closed
      let mut ts' := []
      for (c, ps) in ts do
        let mut ps' := []
        for p in ps do
          let (p', ch) ← widenCopy p
          changed := changed || ch
          ps' := ps' ++ [p']
        ts' := ts' ++ [(c, ps')]
      if !changed then return (r, false)
      let newTail ← if closed then freshFlex else pure tail
      return (← mkStruct (.tags ts' newTail), true)
  | .struct (.nom nm as) _ =>
      if nm == "Try" || nm == "Box" then do
        let mut changed := false
        let mut as' := []
        for a in as do
          let (a', ch) ← widenCopy a
          changed := changed || ch
          as' := as' ++ [a']
        if !changed then return (r, false)
        return (← mkStruct (.nom nm as'), true)
      else return (r, false)
  | _ => return (r, false)

/-! `?` return-row composition (Check.zig `processReturnConstraints`) -/

def tagRowTail (n : Nat) : UM Nat := do
  let (_, e) ← gatherTags n
  return e

/-- Relate each contribution's error row to the composed error row. Heads first, then tails. -/
def composeErr (contribs : List Nat) (errC : Nat) : CM Unit := do
  let tmpl := (← get).tmpl
  let mut tails := []
  for eps in contribs do
    let (ts, tail) ← liftU (gatherTags eps)
    let tc ← liftU (content tail)
    let closed := match tc with | .struct .emptyTags _ => true | _ => false
    if !ts.isEmpty && closed then
      -- R-TryProject: heads copied onto a fresh tail (Check.zig `projectTryReturnRow`)
      let e' ← cflex
      let copy ← cstruct (.tags ts e')
      cunify copy errC
      modify fun s => { s with rels := s.rels.push { tmpl, kind := .tryProject, src := eps, dst := errC } }
    else if !ts.isEmpty then
      -- R-TryWhole: open row with tags, unified whole
      cunify eps errC
    else if !closed then
      tails := tails ++ [eps]
  for eps in tails do
    -- R-TryTail: the composed row's tail is the contribution (Check.zig `processReturnConstraints`)
    let t ← liftU (tagRowTail errC)
    cunify t eps
    modify fun s => { s with rels := s.rels.push { tmpl, kind := .constructionPayload, src := eps, dst := errC } }

/-- The error row of a `Try`-typed var (unifying it with `Try(ok, err)` first). -/
def tryParts (v : Nat) : CM (Nat × Nat) := do
  let ok ← cflex; let err ← cflex
  let t ← cstruct (.nom "Try" [ok, err])
  cunify v t
  return (ok, err)

/-! Inference -/

mutual
partial def infer (env : List (String × Nat)) (e : Expr) : CM Nat := do
  match e with
  | .num _ => cstruct .u64
  | .str _ => cstruct .str
  | .var x =>
      match env.find? (·.1 == x) with
      | some (_, v) => return v
      | none => useGlobal x
  | .add a _ | .sub a _ => do
      let t ← infer env a
      let u ← cstruct .u64
      cunify t u
      return u
  | .list es => do
      let el ← cflex
      for x in es do cunify (← infer env x) el
      cstruct (.list el)
  | .tag c as => do
      let vs ← as.mapM (infer env)
      let ext ← cflex                      -- R-TagOpen: `e_tag` mints a fresh flex ext (Check.zig `resumeTagCheck`)
      cstruct (.tags [(c, vs)] ext)
  | .record fs => do
      let mut out := []
      for (f, x) in fs do out := out ++ [(f, ← infer env x)]
      let e ← cstruct .emptyRec
      cstruct (.record out e)
  | .field r f => do
      let rt ← infer env r
      let a ← cflex; let rho ← cflex
      let want ← cstruct (.record [(f, a)] rho)
      cunify rt want
      return a
  | .call f as => do
      let ft ← infer env f
      let vs ← as.mapM (infer env)
      let ret ← cflex
      let want ← cstruct (.fn vs ret)
      cunify ft want
      return ret
  | .ifEq a _ t f => do
      let aty ← infer env a
      cunify aty (← cstruct .u64)
      let tt ← infer env t
      let ft ← infer env f
      cunify tt ft
      return tt
  | .match_ s arms => do
      let st ← infer env s
      let hasWild := arms.any fun (p, _) => match p with | .wild | .bind _ => true | _ => false
      let mut row := []
      let mut binds : List (List (String × Nat)) := []
      for (p, _) in arms do
        match p with
        | .tag c ps => do
            let vs ← ps.mapM fun _ => cflex
            row := row ++ [(c, vs)]
            binds := binds ++ [(ps.zip vs).filterMap fun (q, v) => match q with | .bind x => some (x, v) | _ => none]
        | .bind x => binds := binds ++ [[(x, st)]]
        | .wild => binds := binds ++ [[]]
      if !row.isEmpty then
        -- R-MatchClose: an exhaustive tag match closes with an uncommitted [] (Check.zig `freshUncommittedTagTail`)
        let tail ← if hasWild then cflex else cfresh (.struct .emptyTags { uncommitted := true })
        let pr ← cstruct (.tags row tail)
        cunify st pr
      let res ← cflex
      for ((_, b), bs) in arms.zip binds do
        cunify (← infer (bs ++ env) b) res
      return res
  | .let_ x v b => do
      let vt ← infer env v
      infer ((x, vt) :: env) b
  | .tryLet x v b => do
      let vt ← infer env v
      let (ok, err) ← tryParts vt
      modify fun s => match s.frames with
        | f :: fs => { s with frames := f.push err :: fs }
        | [] => s
      infer ((x, ok) :: env) b
  | .meth r m as => do
      let rt ← infer env r
      let vs ← as.mapM (infer env)
      let ret ← cflex
      let sig ← cstruct (.fn (rt :: vs) ret)
      let tmpl := (← get).tmpl
      modify fun s => { s with dsites := s.dsites.push { tmpl, recv := rt, meth := m } }
      match (← liftU (content rt)) with
      | .struct ft _ =>
          match ← liftU (methodType ft m) with
          | some mt => cunify mt sig; return ret
          | none => throw s!"missing method {m}"
      | .flex mk =>
          -- R-WhereReq: an unresolved dispatcher carries the requirement
          liftU (setContent rt (.flex { mk with reqs := mk.reqs ++ [{ meth := m, sig }] }))
          return ret
      | _ => throw s!"dispatch on rigid"
  | .lam ps b => inferLam env ps b none

/-- A lambda, optionally against an annotation's function type. -/
partial def inferLam (env : List (String × Nat)) (ps : List String) (b : Expr) (ann : Option Nat) : CM Nat := do
  let pvs ← ps.mapM fun _ => cflex
  modify fun s => { s with frames := #[] :: s.frames }
  let bt ← infer (ps.zip pvs ++ env) b
  let contribs ← do
    let s ← get
    match s.frames with
    | f :: fs => set { s with frames := fs }; pure f.toList
    | [] => pure []
  let annRet ← match ann with
    | some a => do
        match ← liftU (content a) with
        | .struct (.fn as r) _ => do
            for (p, q) in pvs.zip as do cunify p q
            pure (some r)
        | _ => pure none
    | none => pure none
  let tmpl := (← get).tmpl
  if contribs.isEmpty then
    match annRet with
    | some r => do
        -- R-Coerce: the body relates to the annotated result through a re-opened copy
        let (cp, changed) ← liftU (widenCopy bt)
        cunify cp r
        if changed then modify fun s => { s with rels := s.rels.push { tmpl, kind := .resultCoerce, src := bt, dst := r } }
        cstruct (.fn pvs r)
    | none => cstruct (.fn pvs bt)
  else
    -- `?` composition: C is the annotated Try or a fresh Try(body.ok, fresh_err)
    let c ← match annRet with
      | some r => pure r
      | none => do let o ← cflex; let e ← cflex; cstruct (.nom "Try" [o, e])
    let (cok, cerr) ← tryParts c
    let (bok, berr) ← tryParts bt
    cunify bok cok
    composeErr (contribs ++ [berr]) cerr
    cstruct (.fn pvs c)

partial def useGlobal (x : String) : CM Nat := do
  let s ← get
  if let some (_, v) := s.mono.find? (·.1 == x) then
    -- a use of a monomorphic name (group member or top-level value) instantiates nothing
    modify fun s => { s with edges := s.edges.push { tmpl := s.tmpl, callee := x, inst := v, pairs := [], inGroup := s.group.contains x } }
    return v
  match s.schemes.find? (·.1 == x) with
  | some (_, root) => do
      let (v, m) ← liftU (copyGeneric root [])
      modify fun s => { s with edges := s.edges.push { tmpl := s.tmpl, callee := x, inst := v, pairs := m.reverse } }
      return v
  | none =>
      if x == "mk_box" then do
        let a ← cflex; let b ← cstruct (.nom "Box" [a]); cstruct (.fn [a] b)
      else throw s!"unbound {x}"
end

/-! Definitions, binding groups and module end -/

partial def freeGlobals (e : Expr) : List String :=
  match e with
  | .var x => [x]
  | .add a _ | .sub a _ | .field a _ => freeGlobals a
  | .tag _ as | .list as => as.flatMap freeGlobals
  | .record fs => fs.flatMap (freeGlobals ·.2)
  | .call f as => freeGlobals f ++ as.flatMap freeGlobals
  | .ifEq a _ t f => freeGlobals a ++ freeGlobals t ++ freeGlobals f
  | .match_ s arms => freeGlobals s ++ arms.flatMap (freeGlobals ·.2)
  | .let_ _ v b | .tryLet _ v b => freeGlobals v ++ freeGlobals b
  | .meth r _ as => freeGlobals r ++ as.flatMap freeGlobals
  | .lam _ b => freeGlobals b
  | _ => []

def isLam : Expr → Bool
  | .lam .. => true
  | _ => false

/-- Binding groups: a def that references a later def joins one group with it. -/
def groups (defs : List Def) : List (List Def) := Id.run do
  let names := defs.map (·.name)
  let idx (n : String) := names.idxOf n
  let mut out : List (List Def) := []
  let mut i := 0
  while i < defs.length do
    let mut j := i
    let mut k := i
    while k ≤ j do
      for x in freeGlobals defs[k]!.body do
        let t := idx x
        if t < defs.length && t > j then j := t
      k := k + 1
    out := out ++ [(defs.drop i).take (j - i + 1)]
    i := j + 1
  return out

structure Checked where
  cs : CState
  defs : List Def
  /-- expect roots: template name and the type of its left-hand side -/
  roots : List (String × Nat)
  deriving Inhabited

def checkGroup (g : List Def) : CM Unit := do
  modify fun s => { s with st := { s.st with level := 2 }, group := g.map (·.name) }
  -- monomorphic placeholders for the group
  let mut vars := []
  for d in g do
    let v ← cflex
    vars := vars ++ [(d.name, v)]
  modify fun s => { s with mono := vars ++ s.mono }
  for (d, (_, v)) in g.zip vars do
    modify fun s => { s with tmpl := d.name }
    let ann ← match d.ann with
      | some t => do pure (some (← annTy t false d.annMode))
      | none => pure none
    let t ← match d.body, ann with
      | .lam ps b, some a => inferLam [] ps b (some a)
      | body, some a => do let bt ← infer [] body; cunify bt a; pure a
      | body, none => infer [] body
    cunify v t
  modify fun s => { s with mono := s.mono.filter fun (n, _) => !(g.any (·.name == n)), group := [] }
  for (d, (_, v)) in g.zip vars do
    if isLam d.body || d.ann.isSome then
      liftU (generalize v 1)
      modify fun s => { s with schemes := (d.name, v) :: s.schemes }
    else
      -- value restriction: a top-level value is monomorphic and every use shares its rows
      liftU (pinLevel v 1)
      modify fun s => { s with mono := (d.name, v) :: s.mono }
  modify fun s => { s with st := { s.st with level := 1 } }

/-- R-ValueRowClose (Check.zig `closeTopLevelValueRows`): a top-level value's open,
    non-generalized tag tails are closed at module end. -/
def closeValueRows : CM Unit := do
  let s ← get
  for (_, v) in s.mono do
    for r in (← liftU (reachable v)) do
      match ← liftU (content r) with
      | .struct (.tags _ _) _ => do
          let (_, tail) ← liftU (gatherTags r)
          match ← liftU (content tail) with
          | .flex mk => if mk.reqs.isEmpty then cunify tail (← cstruct .emptyTags)
          | _ => pure ()
      | _ => pure ()

def checkProgram (p : Program) : Except String Checked := do
  let st0 : Store := { noms := builtinNoms }
  let act : CM (List (String × Nat)) := do
    for g in groups p.defs do checkGroup g
    -- expects: each `lhs == rhs` is a root template with no generalization
    let mut roots := []
    let mut i := 0
    for (l, r) in p.expects do
      let name := s!"expect#{i}"
      modify fun s => { s with tmpl := name }
      let lt ← infer [] l
      let rt ← infer [] r
      cunify lt rt
      roots := roots ++ [(name, lt)]
      i := i + 1
    closeValueRows
    return roots
  match (act.run { st := st0 }) with
  | .ok (roots, cs) => .ok { cs, defs := p.defs, roots }
  | .error e => .error e

end RE
