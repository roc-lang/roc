/-
  Surface calculus. Every term prints as Roc source (Print.lean) and evaluates (Eval.lean).

  Ground types `G` are what the generator plans with; the model checker (Check.lean)
  re-infers every program from scratch, with open rows, so the plan is only a hint.
-/
namespace RE

/-- Ground (closed) types the generator plans with. -/
inductive G where
  | u64
  | str
  | tags (ts : List (String × List G))
  | record (fs : List (String × G))
  /-- `Try(ok, err)`; `err` is always a `.tags` -/
  | try_ (ok : G) (err : G)
  /-- the user nominal `Nm := [Nm(U64)]` with methods `to_str` and `bump` -/
  | nm
  | fn (args : List G) (ret : G)
  | list (t : G)
  /-- the parameterized nominal `Box(a) := [Box(a)]` with method `unbox` -/
  | box (t : G)
  deriving Repr, BEq, Inhabited

/-- Patterns used in `match` arms. -/
inductive Pat where
  | wild
  | bind (x : String)
  | tag (c : String) (args : List Pat)
  deriving Repr, BEq, Inhabited

inductive Expr where
  | num (n : Nat)
  | str (s : String)
  | var (x : String)
  /-- `a + k` on U64 -/
  | add (a : Expr) (k : Nat)
  /-- `a - k` on U64 (only under `if n == 0 .. else`, so it never underflows) -/
  | sub (a : Expr) (k : Nat)
  | list (es : List Expr)
  | tag (c : String) (args : List Expr)
  | record (fs : List (String × Expr))
  | field (e : Expr) (f : String)
  /-- `f(args)` where `f` is a top-level name or a local -/
  | call (f : Expr) (args : List Expr)
  /-- `if a == k t else e` -/
  | ifEq (a : Expr) (k : Nat) (t e : Expr)
  | match_ (s : Expr) (arms : List (Pat × Expr))
  | let_ (x : String) (v : Expr) (b : Expr)
  /-- `x = v?` then `b` -/
  | tryLet (x : String) (v : Expr) (b : Expr)
  /-- `e.m(args)`: static dispatch -/
  | meth (e : Expr) (m : String) (args : List Expr)
  | lam (ps : List String) (b : Expr)
  deriving Repr, BEq, Inhabited

/-- One top-level definition. -/
structure Def where
  name : String
  /-- printed annotation, if any (`G.fn` for functions) -/
  ann  : Option G
  /-- annotation text overriding `ann` (open rows, holes) -/
  annText : Option String := none
  /-- how the result row of `ann` is written: 0 exact (implicitly open output), 1 `Try(ok, _)`,
      2 `[.., ..]` (open, anonymous tail) -/
  annMode : Nat := 0
  body : Expr
  deriving Repr, Inhabited

structure Program where
  defs    : List Def
  /-- `expect lhs == rhs` -/
  expects : List (Expr × Expr)
  /-- pre-rendered `expect` bodies (codec round trips) -/
  rawExpects : List String := []
  deriving Repr, Inhabited

end RE
