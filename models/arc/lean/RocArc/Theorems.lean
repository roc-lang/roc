/-
  RocArc.Theorems — what the model claims, stated over the executable
  definitions. Proofs are a stretch goal; the statements fix what the bug-finding
  runs check on each input, and what a counterexample to each would mean.

  Every unproven statement carries `sorry`. The ones proven here are the local
  heap facts the bigger proofs would rest on.
-/
import RocArc.Machine
import RocArc.Certify

namespace RocArc

/-! ## Definitions the statements use -/

/-- Every op whose row the model reads behaves as the row says. This is an
    axiom about the Zig builtins (`eval/rc_conformance.zig` tests it); the
    model's own `lowLevel` interprets each row, so for the model this is the
    statement that `lowLevel` and the builtin agree. -/
def OpsConform (_ops : Std.HashMap String RcEffect) : Prop := True

/-- A program certifies: every procedure passes Σ ⊢ p. -/
def Certified (P : Program) (rcl : List Nat) : Prop :=
  ∀ p ∈ P.procs, certifyProc P rcl opMap p = .ok ()

/-- A root is a parameterless procedure. -/
def IsRoot (r : Proc) : Prop := r.params = []

/-- No path from the root reaches a use-after-free, a double free, a leak at
    the root's return, or a stamped in-place write of a shared cell. -/
def MemorySafe (P : Program) (r : Proc) (mode : Mode) (steps paths : Nat) : Prop :=
  (runRoot P r mode steps paths).violations = #[]

/-- What a backend can observe at the end of a path: how it ended, with the
    returned value read through the heap (cell contents, not addresses). -/
partial def observeVal (h : Heap) (fuel : Nat) : Val → String
  | .scalar n => s!"{n}"
  | .uninit => "uninit"
  | .agg t fs => s!"{t}" ++ "(" ++ ",".intercalate (fs.map (observeVal h fuel)) ++ ")"
  | .ptr a =>
    if fuel == 0 then "…" else
    match h[a]? with
    | some c => "[" ++ ",".intercalate (c.children.toList.map (observeVal h (fuel - 1))) ++ "]"
    | none => "dangling"

/-! ## Soundness of the certifier -/

/-- **Certifier soundness.** If every procedure certifies and every op meets its
    row, then no execution of a root reads freed memory, frees twice, or leaks.

    A dump on which the certifier model passes and the machine reports a
    violation is a counterexample: either a certifier rule is missing
    (e.g. #12436, a residual shell passed to a callee that reads the absent
    field, which is outside a procedure-local judgment) or the model's
    abstraction of the rows is wrong. -/
theorem certifier_sound (P : Program) (rcl : List Nat) (r : Proc)
    (_hroot : IsRoot r) (_hmem : r ∈ P.procs)
    (_hcert : Certified P rcl) (_hops : OpsConform opMap) :
    ∀ steps paths, (runRoot P r .checked steps paths).violations = #[] := by
  sorry

/-! ## `unique_args` stamping is unobservable -/

/-- A stamp is justified when, on every path, the stamped argument's cell has
    count 1 when the op runs: exactly the runtime check the stamp removes. -/
def StampsJustified (P : Program) (r : Proc) (steps paths : Nat) : Prop :=
  ∀ v ∈ (runRoot P r .stamped steps paths).violations.toList,
    ¬ (∃ a rc s, v = .sharedMutation a rc s)

/-- **Observational equivalence of stamping.** If every stamp is justified, the
    stamped execution (what the backends run) and the always-check execution
    reach the same outcomes. In this model the two semantics differ only at a
    stamped op, where `stamped` takes the in-place branch unconditionally and
    `checked` takes it iff count == 1; under the hypothesis they take the same
    branch, so the heaps stay equal step for step (a lock-step simulation).

    #12221 (static data on one branch), #12834 and #13031 (a part projected out
    of a shared Dict) are counterexamples to the hypothesis: the stamped run
    writes a cell whose count is 0 (static) or ≥ 2. -/
theorem unique_args_equiv (P : Program) (r : Proc) (steps paths : Nat)
    (_h : StampsJustified P r steps paths) :
    ((runRoot P r .stamped steps paths).violations.size = 0) ↔
    ((runRoot P r .checked steps paths).violations.size = 0) := by
  sorry

/-! ## Proven heap facts -/

/-- Static cells are immortal: `incAddr` never changes them. -/
theorem incAddr_static (s : String) (a n : Nat) (h : Heap) (c : Cell)
    (hc : h[a]? = some c) (hl : c.live = true) (hs : c.rc = 0) :
    ((incAddr s a n).run.run h).2 = h := by
  simp only [incAddr, getCell]
  simp [hc, hl, hs, bind, pure, get, getThe, MonadStateOf.get, ExceptT.run, ExceptT.bind,
    ExceptT.mk, ExceptT.bindCont, ExceptT.pure, StateT.get, StateT.bind, StateT.pure,
    ExceptT.lift, liftM, monadLift, MonadLift.monadLift, StateT.run, Functor.map, StateT.map]

/-! ## In-place reuse (R-Reuse, R-Pure, R-Equiv) -/

namespace Reuse

/-- **Reuse soundness.** If no path of the stamped (in-place) exploration
    reports a violation, in particular no STALE-REUSE (a reference older than
    an in-place write observed the cell) and no SHARED-MUTATION, then every
    path returns the same observable value as the R-Pure execution, where
    every in-place op copies and nothing is freed.

    The proof plan is a simulation: relate each in-place address to its pure
    copy; a forwarded address is dead, so the relation is only broken by a
    touch of the dead address, which the stamped run reports as STALE-REUSE.
    A VALUE-MISMATCH from `rocarc equiv` with no violation is a
    counterexample to this statement, i.e. a model bug (an unconditional
    in-place op the model does not forward). -/
theorem reuse_sound (P : Program) (r : Proc) (steps paths : Nat)
    (_hsafe : (runRoot P r .stamped steps paths).violations = #[])
    (_hsame : (runRoot P r .stamped steps paths).results.size = (runRoot P r .pure steps paths).results.size) :
    (runRoot P r .stamped steps paths).results = (runRoot P r .pure steps paths).results := by
  sorry

set_option linter.unusedSimpArgs false

/-- `forward` moves a live cell to a fresh address with its count and
    contents unchanged, and leaves the old address dead and pointing at it. -/
theorem forward_moves (h : Heap) (a : Nat) (c : Cell) (hc : h[a]? = some c) (_ha : a < h.size) :
    ((forward a).run.run h) =
      (.ok h.size, (h.push c).set! a { c with live := false, reusedAs := some h.size }) := by
  simp [forward, getCell, setCell, hc, ExceptT.run, bind, ExceptT.bind, ExceptT.mk,
    ExceptT.bindCont, get, getThe, MonadStateOf.get, StateT.get, set, StateT.set,
    modify, modifyGet, MonadStateOf.modifyGet, StateT.modifyGet, pure, ExceptT.pure,
    StateT.bind, StateT.pure, StateT.run, ExceptT.lift, liftM, monadLift, MonadLift.monadLift]
  simp [Functor.map, StateT.map, StateT.get, StateT.set, StateT.bind, StateT.pure, StateT.modifyGet,
    ExceptT.bindCont, hc, Id.run]
  simp [pure, hc, StateT.map, StateT.get, StateT.set, StateT.bind, StateT.modifyGet, ExceptT.bindCont, StateT.pure, bind]

end Reuse

end RocArc
