import RowsEvidence.Syntax
import RowsEvidence.Eval
/-!
  Type-directed random composition of small typed constructs.

  The generator plans each term at a ground type, but deliberately lets a value of a
  narrower row flow into a wider one (`fits`), at every position the checker's coercion
  reach covers and some it does not. Whether the checker accepts is decided by `roc check`
  and by the model checker; the plan is only how terms are composed.
-/
namespace RE

structure GState where
  seed  : Nat
  fresh : Nat := 0
  /-- generic helpers the program uses -/
  used  : List String := []

abbrev GM := StateM GState

def rnd (n : Nat) : GM Nat := do
  let s ← get
  let x := (s.seed * 6364136223846793005 + 1442695040888963407) % 18446744073709551616
  set { s with seed := x }
  return if n == 0 then 0 else (x / 65536) % n

def chance (pct : Nat) : GM Bool := do return (← rnd 100) < pct

def pick {α} [Inhabited α] (xs : List α) : GM α := do
  return xs[(← rnd xs.length)]!

def freshName (p : String) : GM String := do
  let s ← get
  set { s with fresh := s.fresh + 1 }
  return s!"{p}{s.fresh}"

def use (h : String) : GM Unit := modify fun s => if s.used.contains h then s else { s with used := s.used ++ [h] }

/-! Ground type helpers -/

def tagPool : List String := ["A", "B", "C", "Oops", "Bad", "Wrap", "Early", "Zero"]

/-- `have` may flow into `want` by row widening (structurally, at any depth). -/
partial def fits : G → G → Bool
  | .tags hs, .tags ws => hs.all fun (c, ps) => match ws.find? (·.1 == c) with
      | some (_, qs) => ps.length == qs.length && (ps.zip qs).all fun (p, q) => fits p q
      | none => false
  | .try_ a e, .try_ b f => fits a b && fits e f
  | .record fs, .record gs => fs.length == gs.length && fs.all fun (f, t) => (gs.find? (·.1 == f)).any (·.2 == t)
  | a, b => a == b

partial def G.hasNm : G → Bool
  | .nm => true
  | .box _ => true
  | .list t => t.hasNm
  | .tags ts => ts.any fun (_, ps) => ps.any G.hasNm
  | .record fs => fs.any fun (_, t) => t.hasNm
  | .try_ a e => a.hasNm || e.hasNm
  | _ => false

partial def G.hasFn : G → Bool
  | .fn .. => true
  | .tags ts => ts.any fun (_, ps) => ps.any G.hasFn
  | .record fs => fs.any fun (_, t) => t.hasFn
  | .try_ a e => a.hasFn || e.hasFn
  | .list t | .box t => t.hasFn
  | _ => false

mutual
partial def randTags (depth : Nat) : GM G := do
  let k := 1 + (← rnd 3)
  let mut ts : List (String × List G) := []
  for _ in [0:k] do
    let c ← pick tagPool
    if ts.any (·.1 == c) then continue
    let r2 ← rnd 2
    let np := if depth == 0 || c == "Zero" || c == "Early" then 0 else r2
    let mut ps := []
    for _ in [0:np] do ps := ps ++ [← randG (depth - 1)]
    ts := ts ++ [(c, ps)]
  return .tags ts

partial def randG (depth : Nat) : GM G := do
  match ← rnd (if depth == 0 then 3 else 9) with
  | 0 => return .u64
  | 1 => return .str
  | 2 => return .u64
  | 3 => randTags depth
  | 4 => return .try_ (← randG (depth - 1)) (← randTags (depth - 1))
  | 5 => return .record [("val", ← randG (depth - 1)), ("n", .u64)]
  | 6 => return .list (← randG (depth - 1))
  | 7 => randTags depth
  | _ => return .nm
end

/-- A strictly narrower version of a type (drop tags, recursively), for widening flows. -/
partial def narrow (g : G) : GM G := do
  match g with
  | .tags ts =>
      let keep ← ts.filterM fun _ => do return (← rnd 3) != 0
      let keep := if keep.isEmpty then ts.take 1 else keep
      let keep ← keep.mapM fun (c, ps) => do return (c, ← ps.mapM narrow)
      return .tags keep
  | .try_ a e => return .try_ (← narrow a) (← narrow e)
  | g => return g

/-! Plans of user definitions -/

structure DefPlan where
  name   : String
  params : List G
  ret    : G
  /-- a top-level value (record of closures) rather than a function -/
  isValue : Bool := false
  /-- a top-level constant (no parameters) -/
  isConst : Bool := false
  /-- in a recursive group: callable only as `f(n - 1, ..)` under `if n == 0 .. else` -/
  isRec : Bool := false
  deriving Inhabited

structure Ctx where
  locals : List (String × G) := []
  defs   : List DefPlan := []
  /-- the enclosing function's `Try` error row, when `?` is allowed here -/
  tryErr : Option G := none
  /-- the `U64` parameter in scope, used as the branch scrutinee -/
  scrut  : Option String := none
  /-- inside the `else` of `if n == 0`: recursive calls at `n - 1` are allowed -/
  allowRec : Bool := false

mutual
/-- Generate an expression of (a type that fits into) `g`. -/
partial def gen (cx : Ctx) (g : G) (depth : Nat) : GM Expr := do
  let base ← genBase cx g depth
  if depth == 0 then return base
  match ← rnd 12 with
  | 0 => -- local, or top-level constant, of a fitting type
      let consts := cx.defs.filter (fun d => d.isConst) |>.map fun d => (d.name, d.ret)
      match (cx.locals ++ consts).filter (fun (_, t) => fits t g) with
      | [] => return base
      | ls => do let (x, _) ← pick ls; return .var x
  | 1 => -- call a user definition whose result fits
      let cands := cx.defs.filter fun d => !d.isValue && !d.isConst && fits d.ret g && (!d.isRec || cx.allowRec)
      match cands with
      | [] => return base
      | ds => do
          let d ← pick ds
          let args ← d.params.mapM (gen { cx with tryErr := none, allowRec := false } · (depth - 1))
          let args := if d.isRec then (match cx.scrut with
            | some n => Expr.sub (.var n) 1 :: args.drop 1
            | none => args) else args
          return .call (.var d.name) args
  | 2 => -- branch; the `else` of `if n == 0` may recurse
      match cx.scrut with
      | some n => do
          let k ← rnd 3
          let t ← gen cx g (depth - 1)
          let e ← gen (if k == 0 then { cx with allowRec := true } else cx) g (depth - 1)
          return .ifEq (.var n) k t e
      | none => return base
  | 3 => -- let-bound intermediate of a narrower type, then use it
      let x ← freshName "v"
      let t ← narrow g
      let v ← gen cx t (depth - 1)
      return .let_ x v (← gen { cx with locals := (x, t) :: cx.locals } g (depth - 1))
  | 4 => -- `?` on a Try whose error row fits the enclosing result
      match cx.tryErr with
      | some err => do
          let x ← freshName "t"
          let okT ← pick [.u64, .str, ← randTags 0]
          let errT ← narrow err
          let v ← gen { cx with tryErr := none } (.try_ okT errT) (depth - 1)
          return .tryLet x v (← gen { cx with locals := (x, okT) :: cx.locals } g (depth - 1))
      | none => return base
  | 5 => -- generic identity / pick
      if ← chance 50 then do use "idv"; return .call (.var "idv") [← gen cx g (depth - 1)]
      else
        match cx.scrut with
        | some n => do use "pick"; return .call (.var "pick") [.var n, ← gen cx g (depth - 1), ← gen cx g (depth - 1)]
        | none => return base
  | 6 => -- through a record field generically, or through the parameterized nominal
      if ← chance 50 then
        use "getv"
        return .call (.var "getv") [.record [("val", ← gen cx g (depth - 1)), ("n", .num (← rnd 5))]]
      else
        return .meth (.call (.var "mk_box") [← gen cx g (depth - 1)]) "unbox" []
  | 7 => -- apply a lambda
      use "apply"
      let x ← freshName "a"
      let argT ← pick [.u64, g]
      let body ← gen { cx with locals := (x, argT) :: cx.locals, tryErr := none } g (depth - 1)
      return .call (.var "apply") [.lam [x] body, ← gen { cx with tryErr := none } argT (depth - 1)]
  | 8 => -- call through a record of closures (a value definition)
      let cands := cx.defs.filter fun d => d.isValue && fits d.ret g
      match cands with
      | [] => return base
      | ds => do
          let d ← pick ds
          return .call (.field (.var d.name) "run") [← gen { cx with tryErr := none } .u64 (depth - 1)]
  | 9 => -- match on a tag value, every arm producing `g`
      let st ← randTags 1
      let s ← gen { cx with tryErr := none } st (depth - 1)
      match st with
      | .tags ts => do
          let mut arms := []
          for (c, ps) in ts do
            let xs ← ps.mapM fun _ => freshName "p"
            let bindIt ← chance 30
            let locals := if bindIt then (xs.zip ps) ++ cx.locals else cx.locals
            let pats := if bindIt then xs.map Pat.bind else xs.map fun _ => Pat.wild
            arms := arms ++ [(Pat.tag c pats, ← gen { cx with locals } g (depth - 1))]
          return .match_ s arms
      | _ => return base
  | _ => return base

/-- Constructions that build a value of exactly `g`. -/
partial def genBase (cx : Ctx) (g : G) (depth : Nat) : GM Expr := do
  let d1 := if depth == 0 then 0 else depth - 1
  match g with
  | .u64 =>
      if depth > 0 && (← chance 30) then return .add (← gen cx .u64 d1) (← rnd 4)
      match cx.locals.filter (·.2 == .u64) with
      | [] => return .num (← rnd 5)
      | ls => if ← chance 50 then do let (x, _) ← pick ls; return .var x else return .num (← rnd 5)
  | .str =>
      match ← rnd 5 with
      | 4 => return .meth (← gen cx .nm d1) "label" []
      | 0 => return .meth (← gen cx .u64 d1) "to_str" []
      | 1 => do use "show"; return .call (.var "show") [← gen cx (← pick [.u64, .nm]) d1]
      | 2 => return .meth (← gen cx .nm d1) "to_str" []
      | _ => return .str (← pick ["x", "y", "hi"])
  | .nm =>
      match ← rnd 3 with
      | 0 => do use "bumpit"; return .call (.var "bumpit") [← gen cx .nm d1]
      | 1 => return .meth (← gen cx .nm d1) "bump" []
      | _ => do use "mk_nm"; return .call (.var "mk_nm") [← gen cx .u64 d1]
  | .tags ts =>
      let (c, ps) ← pick ts
      if c == "Wrap" && ps.length == 1 && (← chance 50) then
        use "wrap"; return .call (.var "wrap") [← gen cx ps[0]! d1]
      return .tag c (← ps.mapM (gen cx · d1))
  | .try_ ok err =>
      match ← rnd 6 with
      | 0 => do use "pass"; return .call (.var "pass") [← gen { cx with tryErr := none } g d1]
      | 1 => do
          use "mapok"
          let x ← freshName "m"
          return .call (.var "mapok") [← gen { cx with tryErr := none } (.try_ .u64 err) d1,
            .lam [x] (← gen { cx with locals := (x, .u64) :: cx.locals, tryErr := none } ok d1)]
      | 2 => match err with
          | .tags ts => do
              let (c, ps) ← pick ts
              return .tag "Err" [.tag c (← ps.mapM (gen cx · d1))]
          | _ => return .tag "Ok" [← gen cx ok d1]
      | 3 => if ok == .u64 && (match err with | .tags ts => ts.any (fun p => p.1 == "Zero" && p.2.isEmpty) | _ => false) then do
               use "adderr"; return .call (.var "adderr") [← gen { cx with tryErr := none } (.try_ .u64 (← narrow err)) d1]
             else return .tag "Ok" [← gen cx ok d1]
      | _ => return .tag "Ok" [← gen cx ok d1]
  | .record fs => return .record (← fs.mapM fun (f, t) => do return (f, ← gen cx t d1))
  | .list t => do
      let k ← rnd 3
      let mut es := []
      for _ in [0:k] do es := es ++ [← gen cx t d1]
      return .list es
  | .box t => return .call (.var "mk_box") [← gen cx t d1]
  | .fn as r =>
      let xs ← as.mapM fun _ => freshName "q"
      return .lam xs (← gen { cx with locals := xs.zip as ++ cx.locals, tryErr := none } r d1)
end

/-! Generic helpers: small polymorphic constructs, each instantiated wherever the generator calls it. -/

def helperDefs : List Def := [
  { name := "idv", ann := none, body := .lam ["x"] (.var "x") },
  { name := "pick", ann := none, body := .lam ["n", "a", "b"] (.ifEq (.var "n") 0 (.var "a") (.var "b")) },
  { name := "pass", ann := none, body := .lam ["t"] (.tryLet "v" (.var "t") (.tag "Ok" [.var "v"])) },
  { name := "wrap", ann := none, body := .lam ["x"] (.tag "Wrap" [.var "x"]) },
  { name := "show", ann := none, body := .lam ["x"] (.meth (.var "x") "to_str" []) },
  { name := "mapok", ann := none, body := .lam ["t", "f"] (.match_ (.var "t")
      [(.tag "Ok" [.bind "v"], .tag "Ok" [.call (.var "f") [.var "v"]]),
       (.tag "Err" [.bind "e"], .tag "Err" [.var "e"])]) },
  { name := "apply", ann := none, body := .lam ["f", "x"] (.call (.var "f") [.var "x"]) },
  { name := "getv", ann := none, body := .lam ["r"] (.field (.var "r") "val") },
  { name := "bumpit", ann := none, body := .lam ["x"] (.meth (.var "x") "bump" []) },
  { name := "adderr", ann := none, body := .lam ["t"] (.tryLet "v" (.var "t")
      (.ifEq (.var "v") 0 (.tag "Err" [.tag "Zero" []]) (.tag "Ok" [.var "v"]))) },
  { name := "mk_nm", ann := some (.fn [.u64] .nm), body := .lam ["n"] (.tag "Nm" [.var "n"]) }
]

/-- `[A, B(U64), ..]`: an annotation row left open. -/
def openTags (ts : List (String × List G)) : String :=
  let r := (G.tags ts).roc
  String.ofList (r.toList.take (r.length - 1))

/-- Generate a whole program. -/
def genProgram (seed : Nat) (ndefs : Nat := 4) (depth : Nat := 3) : Program := Id.run do
  let act : GM (List Def × List DefPlan) := do
    let mut defs : List Def := []
    let mut plans : List DefPlan := []
    for i in [0:ndefs] do
      let ret ← randG 2
      if i > 0 && (← chance 12) then
        -- a top-level constant: its rows are value rows that every use shares
        let name := s!"c{i}"
        let body ← gen { defs := plans } ret depth
        defs := defs ++ [{ name, ann := none, body }]
        plans := plans ++ [{ name, params := [], ret, isConst := true }]
        continue
      let isValue := i > 0 && (← chance 15)
      if isValue then
        -- `hooks = { run: |n| ... }`: a record holding a closure
        let name := s!"hooks{i}"
        let body ← gen { defs := plans, scrut := some "n", locals := [("n", .u64)] } ret depth
        defs := defs ++ [{ name, ann := none, body := .record [("run", .lam ["n"] body)] }]
        plans := plans ++ [{ name, params := [.u64], ret, isValue := true }]
        continue
      let wantExtra ← chance 40
      let extraT ← randG 1
      let extra : Option G := if wantExtra then some extraT else none
      let params := [G.u64] ++ (match extra with | some t => [t] | none => [])
      let names := ["n"] ++ (match extra with | some _ => ["x"] | none => [])
      let tryErr := match ret with | .try_ _ e => some e | _ => none
      let name := s!"f{i}"
      let recursive ← chance 25
      let selfPlan : DefPlan := { name, params, ret, isRec := true }
      let cx : Ctx := { locals := names.zip params, defs := (if recursive then plans ++ [selfPlan] else plans), tryErr, scrut := some "n" }
      let body ← if recursive then do
          -- `if n == 0 base else step`, where only `step` may call `name(n - 1, ..)`
          let base ← gen { cx with defs := plans } ret depth
          let step ← gen { cx with allowRec := true } ret depth
          pure (Expr.ifEq (.var "n") 0 base step)
        else gen cx ret depth
      let annotated ← chance 55
      let mode ← rnd 3
      let annText : Option String := match ret, mode with
        | .try_ ok _, 1 => some s!"{", ".intercalate (params.map G.roc)} -> Try({ok.roc}, _)"
        | .try_ ok (.tags ts), 2 =>
            some s!"{", ".intercalate (params.map G.roc)} -> Try({ok.roc}, {openTags ts}, ..])"
        | .tags ts, 2 => some s!"{", ".intercalate (params.map G.roc)} -> {openTags ts}, ..]"
        | _, _ => none
      defs := defs ++ [{ name, ann := if annotated && !ret.hasFn then some (.fn params ret) else none,
                         annText := if annotated && !ret.hasFn then annText else none,
                         annMode := if annotated && !ret.hasFn && annText.isSome then mode else 0,
                         body := .lam names body }]
      plans := plans ++ [{ name, params, ret }]
      -- a point-free alias of a definition, sometimes
      if ← chance 20 then
        let aname := s!"alias{i}"
        defs := defs ++ [{ name := aname, ann := none, body := .var name }]
        plans := plans ++ [{ name := aname, params, ret }]
    return (defs, plans)
  let ((defs, plans), st) := (act.run { seed }).run
  -- pull in the helpers that were used (helpers never call user defs)
  let helpers := helperDefs.filter fun h => st.used.contains h.name
  let allDefs := helpers ++ defs
  -- expects: call every function definition at a few arguments; the reference semantics gives the value
  let mut expects : List (Expr × Expr) := []
  let mut raws : List String := []
  let mut s2 : GState := { seed := seed + 7 }
  for pl in plans do
    if pl.ret.hasNm then continue
    for k in [0, 1, 2] do
      let (args, s3) := (pl.params.mapM (fun t => gen {} t 1)).run s2
      s2 := s3
      let lhs := if pl.isValue then Expr.call (.field (.var pl.name) "run") [.num k]
                 else if pl.isConst then Expr.var pl.name
                 else Expr.call (.var pl.name) ((Expr.num k) :: args.drop 1)
      -- helper uses inside expect arguments must be declared too
      match evalTop { defs := allDefs ++ helperDefs.filter (fun h => !(helpers.any (·.name == h.name))), expects := [] } lhs with
      | .ok v => match v.toExpr with
          | some rhs =>
              expects := expects ++ [(lhs, rhs)]
              -- codec round trip at the planned type (derived encoder and parser contracts)
              if k == 0 && !pl.ret.hasFn && pl.ret != .nm then
                raws := raws ++ [s!"\{\n    v = {lhs.roc 1}\n    r : Try({pl.ret.roc}, [InvalidJson(Str)])\n    r = Json.parse(Json.to_str(v))\n    r == Ok(v)\n}"]
          | none => pure ()
      | .error _ => pure ()
  let used2 := (helperDefs.filter fun h => s2.used.contains h.name && !(helpers.any (·.name == h.name)))
  return { defs := helpers ++ used2 ++ defs, expects, rawExpects := raws }

end RE
