/-
Mechanized core of "Atomic Reference Counts Across Tasks"
(design/language/concurrency-rc-atomicity.md), in Lean 4 core (no Mathlib).

* Theorem B: the reachability closure of the mode constraints is a solution,
  is below every other solution, and so minimizes every monotone cost.
* Theorem A: within one fork window, if a mode predicate satisfies the
  constraints of every party's code and seeds every capture that reaches a
  shared object, then
  - any object two different parties both touch is shared, and
  - every touch of a shared object goes through a (local, path) that the
    predicate marks atomic.

The window model: parties start from one immutable initial heap and their
captures, allocate only fresh objects tagged with their own id, and never
exchange handles until the join. In-place updates are omitted; they touch
only unique objects, which no other party can reach (the RFC's L1), and the
Maude model checks them.
-/

namespace RcAtomicity

abbrev Label := Nat
abbrev Path := List Label
abbrev Local := Nat
abbrev PartyId := Nat

/-! ## Theorem B: least solutions of monotone constraint systems -/

section Least

inductive Reach {α : Type} (edge : α → α → Prop) (seed : α → Prop) : α → Prop
  | seed {a} : seed a → Reach edge seed a
  | step {a b} : Reach edge seed a → edge a b → Reach edge seed b

def IsSolution {α : Type} (edge : α → α → Prop) (seed : α → Prop) (S : α → Prop) : Prop :=
  (∀ a, seed a → S a) ∧ (∀ a b, S a → edge a b → S b)

theorem reach_isSolution {α : Type} (edge : α → α → Prop) (seed : α → Prop) :
    IsSolution edge seed (Reach edge seed) :=
  ⟨fun _ h => .seed h, fun _ _ h e => .step h e⟩

theorem reach_least {α : Type} (edge : α → α → Prop) (seed : α → Prop)
    (S : α → Prop) (hS : IsSolution edge seed S) :
    ∀ a, Reach edge seed a → S a := by
  intro a h
  induction h with
  | seed h => exact hS.1 _ h
  | step _ e ih => exact hS.2 _ _ ih e

/-- Theorem B: no solution costs less than the least one, for any cost that
is monotone in the set of atomic nodes. -/
theorem reach_optimal {α : Type} (edge : α → α → Prop) (seed : α → Prop)
    (cost : (α → Prop) → Nat)
    (mono : ∀ S T : α → Prop, (∀ a, S a → T a) → cost S ≤ cost T)
    (S : α → Prop) (hS : IsSolution edge seed S) :
    cost (Reach edge seed) ≤ cost S :=
  mono _ _ (reach_least edge seed S hS)

end Least

/-! ## The window model -/

inductive Obj where
  | init : Nat → Obj
  | fresh : PartyId → Nat → Obj
  deriving DecidableEq

abbrev Heap := Obj → Label → Option Obj

def deref (h : Heap) : Obj → Path → Option Obj
  | o, [] => some o
  | o, l :: p =>
    match h o l with
    | none => none
    | some o' => deref h o' p

def findField : Label → List (Label × Local) → Option Local
  | _, [] => none
  | l, (k, y) :: rest => if l = k then some y else findField l rest

theorem findField_mem : ∀ {l : Label} {fs : List (Label × Local)} {y : Local},
    findField l fs = some y → (l, y) ∈ fs
  | _, [], _, h => by simp [findField] at h
  | l, (k, z) :: rest, y, h => by
    simp only [findField] at h
    by_cases hlk : l = k
    · simp [hlk] at h
      subst hlk; subst h
      exact List.Mem.head _
    · simp [hlk] at h
      exact List.Mem.tail _ (findField_mem h)

inductive Stmt where
  | mk : Local → List (Label × Local) → Stmt   -- x := C(l1: y1, ...)
  | cp : Local → Local → Stmt                  -- x := y
  | proj : Local → Local → Label → Stmt        -- x := y.l
  | touch : Local → Path → Stmt                -- a count update through x at path p

abbrev Env := Local → Option Obj

structure St where
  heap : Heap
  env : Env
  next : Nat

def setEnv (e : Env) (x : Local) (v : Option Obj) : Env :=
  fun y => if y = x then v else e y

def mkFields (e : Env) (fs : List (Label × Local)) (l : Label) : Option Obj :=
  match findField l fs with
  | some y => e y
  | none => none

def projOf (s : St) (y : Local) (l : Label) : Option Obj :=
  match s.env y with
  | some o => s.heap o l
  | none => none

/-- An event: a count update through local `x` at path `p` on object `o`. -/
abbrev Event := Local × Path × Obj

def touchEvents (s : St) (x : Local) (p : Path) : List Event :=
  match s.env x with
  | some o =>
    match deref s.heap o p with
    | some o' => [(x, p, o')]
    | none => []
  | none => []

def exec (pid : PartyId) (s : St) : Stmt → St × List Event
  | .mk x fs =>
    ({ heap := fun o l => if o = Obj.fresh pid s.next then mkFields s.env fs l else s.heap o l,
       env := setEnv s.env x (some (Obj.fresh pid s.next)),
       next := s.next + 1 }, [])
  | .cp x y => ({ s with env := setEnv s.env x (s.env y) }, [])
  | .proj x y l => ({ s with env := setEnv s.env x (projOf s y l) }, [])
  | .touch x p => (s, touchEvents s x p)

def run (pid : PartyId) : St → List Stmt → List Event
  | _, [] => []
  | s, st :: rest => (exec pid s st).2 ++ run pid (exec pid s st).1 rest

/-! ## Mode constraints -/

abbrev Modes := Local → Path → Prop

def StmtClosed (M : Modes) : Stmt → Prop
  | .mk x fs => ∀ l y p, (l, y) ∈ fs → M y p → M x (l :: p)
  | .cp x y => ∀ p, M y p → M x p
  | .proj x y l => ∀ p, M y (l :: p) → M x p
  | .touch _ _ => True

def Mono (M : Modes) : Prop := ∀ x p l, M x p → M x (p ++ [l])

def Closed (M : Modes) (code : List Stmt) : Prop :=
  Mono M ∧ ∀ st ∈ code, StmtClosed M st

/-- The constraint graph of a program, over (local, path) nodes. -/
inductive CodeEdge (code : List Stmt) : (Local × Path) → (Local × Path) → Prop
  | mono {x p l} : CodeEdge code (x, p) (x, p ++ [l])
  | cp {x y p} : Stmt.cp x y ∈ code → CodeEdge code (y, p) (x, p)
  | proj {x y l p} : Stmt.proj x y l ∈ code → CodeEdge code (y, l :: p) (x, p)
  | mk {x fs l y p} : Stmt.mk x fs ∈ code → (l, y) ∈ fs → CodeEdge code (y, p) (x, l :: p)

/-- The least solution (forward reachability from the seeds) satisfies the
constraints, so Theorem A applies to it. -/
theorem reach_closed (code : List Stmt) (seeds : Local × Path → Prop) :
    Closed (fun x p => Reach (CodeEdge code) seeds (x, p)) code := by
  refine ⟨fun x p l h => .step h .mono, ?_⟩
  intro st hst
  cases st with
  | mk x fs => exact fun l y p hm h => .step h (.mk hst hm)
  | cp x y => exact fun p h => .step h (.cp hst)
  | proj x y l => exact fun p h => .step h (.proj hst)
  | touch _ _ => trivial

theorem mono_append {M : Modes} (hM : Mono M) :
    ∀ (q : Path) (x : Local) (p : Path), M x p → M x (p ++ q)
  | [], _, p, h => by simpa using h
  | l :: q, x, p, h => by
    have h1 : M x (p ++ [l]) := hM x p l h
    have h2 := mono_append hM q x (p ++ [l]) h1
    simpa [List.append_assoc] using h2

theorem closed_tail {M : Modes} {st : Stmt} {rest : List Stmt}
    (h : Closed M (st :: rest)) : Closed M rest :=
  ⟨h.1, fun s hs => h.2 s (List.Mem.tail _ hs)⟩

theorem closed_head {M : Modes} {st : Stmt} {rest : List Stmt}
    (h : Closed M (st :: rest)) : StmtClosed M st :=
  h.2 st (List.Mem.head _)

/-! ## Heap facts -/

theorem deref_append (h : Heap) : ∀ (o : Obj) (p : Path) (l : Label),
    deref h o (p ++ [l]) = (match deref h o p with
      | some o' => h o' l
      | none => none)
  | o, [], l => by
    simp only [List.nil_append, deref]
    cases h o l <;> rfl
  | o, k :: p, l => by
    simp only [List.cons_append, deref]
    cases h o k with
    | none => rfl
    | some o' => exact deref_append h o' p l

section Window

variable (h0 : Heap) (caps : PartyId → Obj → Prop)

/-- Reachable at the fork from one of party `P`'s captures. -/
def R0 (P : PartyId) (o : Obj) : Prop :=
  ∃ c p, caps P c ∧ deref h0 c p = some o

/-- Reachable at the fork by two different parties. -/
def Shared (o : Obj) : Prop :=
  ∃ P Q, P ≠ Q ∧ R0 h0 caps P o ∧ R0 h0 caps Q o

/-- What a party can reach: its own fresh objects, or initial objects
reachable from its captures. -/
def Prov (P : PartyId) (o : Obj) : Prop :=
  (∃ i, o = Obj.fresh P i) ∨ (∃ n, o = Obj.init n ∧ R0 h0 caps P o)

def Mine (P : PartyId) (s : St) (o : Obj) : Prop :=
  (∃ i, o = Obj.fresh P i ∧ i < s.next) ∨ (∃ n, o = Obj.init n ∧ R0 h0 caps P o)

/-- Assumptions on the initial heap: initial objects point only to initial
objects, and there are no fresh objects yet. -/
structure HeapOk : Prop where
  initClosed : ∀ n l o, h0 (Obj.init n) l = some o → ∃ m, o = Obj.init m
  freshEmpty : ∀ q i l, h0 (Obj.fresh q i) l = none
  capsInit : ∀ P c, caps P c → ∃ n, c = Obj.init n

theorem deref_init_closed (hh : HeapOk h0 caps) :
    ∀ (p : Path) (n : Nat) (o : Obj), deref h0 (Obj.init n) p = some o → ∃ m, o = Obj.init m
  | [], n, o, h => by
    simp [deref] at h
    exact ⟨n, h.symm⟩
  | l :: p, n, o, h => by
    simp only [deref] at h
    cases hl : h0 (Obj.init n) l with
    | none => rw [hl] at h; simp at h
    | some o' =>
      rw [hl] at h
      obtain ⟨m, rfl⟩ := hh.initClosed n l o' hl
      exact deref_init_closed hh p m o h

theorem R0_init (hh : HeapOk h0 caps) {P : PartyId} {o : Obj} (h : R0 h0 caps P o) :
    ∃ n, o = Obj.init n := by
  obtain ⟨c, p, hc, hd⟩ := h
  obtain ⟨n, rfl⟩ := hh.capsInit P c hc
  exact deref_init_closed h0 caps hh p n o hd

theorem fresh_not_shared (hh : HeapOk h0 caps) (P : PartyId) (i : Nat) :
    ¬ Shared h0 caps (Obj.fresh P i) := by
  intro ⟨A, _, _, hA, _⟩
  obtain ⟨n, hn⟩ := R0_init h0 caps hh hA
  cases hn

theorem R0_step {P : PartyId} {o o' : Obj} {l : Label}
    (h : R0 h0 caps P o) (hf : h0 o l = some o') : R0 h0 caps P o' := by
  obtain ⟨c, p, hc, hd⟩ := h
  refine ⟨c, p ++ [l], hc, ?_⟩
  rw [deref_append, hd]
  exact hf

/-- The invariant of one party's run. -/
structure Good (P : PartyId) (M : Modes) (s : St) : Prop where
  initFrozen : ∀ n l, s.heap (Obj.init n) l = h0 (Obj.init n) l
  fieldsMine : ∀ o l o', Mine h0 caps P s o → s.heap o l = some o' → Mine h0 caps P s o'
  envMine : ∀ x o, s.env x = some o → Mine h0 caps P s o
  modes : ∀ x o p o', s.env x = some o → deref s.heap o p = some o' →
    Shared h0 caps o' → M x p

theorem mine_prov {P : PartyId} {s : St} {o : Obj} (h : Mine h0 caps P s o) :
    Prov h0 caps P o := by
  rcases h with ⟨i, rfl, _⟩ | ⟨n, rfl, hr⟩
  · exact Or.inl ⟨i, rfl⟩
  · exact Or.inr ⟨n, rfl, hr⟩

theorem mine_mono {P : PartyId} {s s' : St} {o : Obj} (hn : s.next ≤ s'.next)
    (h : Mine h0 caps P s o) : Mine h0 caps P s' o := by
  rcases h with ⟨i, rfl, hi⟩ | h
  · exact Or.inl ⟨i, rfl, Nat.lt_of_lt_of_le hi hn⟩
  · exact Or.inr h

theorem deref_mine {P : PartyId} {M : Modes} {s : St} (g : Good h0 caps P M s) :
    ∀ (p : Path) (o o' : Obj), Mine h0 caps P s o → deref s.heap o p = some o' →
      Mine h0 caps P s o'
  | [], o, o', hm, hd => by
    simp [deref] at hd
    subst hd; exact hm
  | l :: p, o, o', hm, hd => by
    simp only [deref] at hd
    cases hl : s.heap o l with
    | none => rw [hl] at hd; simp at hd
    | some o'' =>
      rw [hl] at hd
      exact deref_mine g p o'' o' (g.fieldsMine o l o'' hm hl) hd

/-- Allocating the next fresh object does not change any path from an
object the party could already reach. -/
theorem deref_frame {P : PartyId} {M : Modes} {s : St} (g : Good h0 caps P M s)
    (f : Label → Option Obj) :
    ∀ (p : Path) (o : Obj), Mine h0 caps P s o →
      deref (fun o' l => if o' = Obj.fresh P s.next then f l else s.heap o' l) o p
        = deref s.heap o p
  | [], o, _ => rfl
  | l :: p, o, hm => by
    have hne : o ≠ Obj.fresh P s.next := by
      intro he; subst he
      rcases hm with ⟨i, hi, hlt⟩ | ⟨n, hn, _⟩
      · injection hi with _ hi2
        omega
      · cases hn
    simp only [deref, hne, ite_false]
    cases hl : s.heap o l with
    | none => rfl
    | some o' =>
      exact deref_frame g f p o' (g.fieldsMine o l o' hm hl)

theorem good_init (hh : HeapOk h0 caps) (P : PartyId) (M : Modes) (hM : Mono M)
    (env0 : Env)
    (henv : ∀ x c, env0 x = some c → caps P c)
    (hseed : ∀ x c, env0 x = some c →
      (∃ p o, deref h0 c p = some o ∧ Shared h0 caps o) → M x []) :
    Good h0 caps P M { heap := h0, env := env0, next := 0 } := by
  refine ⟨fun _ _ => rfl, ?_, ?_, ?_⟩
  · intro o l o' hm hf
    rcases hm with ⟨i, _, hi⟩ | ⟨n, rfl, hr⟩
    · exact absurd hi (Nat.not_lt_zero _)
    · obtain ⟨m, rfl⟩ := hh.initClosed n l o' hf
      exact Or.inr ⟨m, rfl, R0_step h0 caps hr hf⟩
  · intro x o he
    have hc := henv x o he
    obtain ⟨n, rfl⟩ := hh.capsInit P o hc
    exact Or.inr ⟨n, rfl, ⟨Obj.init n, [], hc, rfl⟩⟩
  · intro x o p o' he hd hs
    have h0' : M x [] := hseed x o he ⟨p, o', hd, hs⟩
    simpa using mono_append hM p x [] h0'

theorem good_exec (hh : HeapOk h0 caps) {P : PartyId} {M : Modes} {s : St}
    (g : Good h0 caps P M s) (_hM : Mono M) (st : Stmt) (hst : StmtClosed M st) :
    Good h0 caps P M (exec P s st).1 := by
  cases st with
  | touch x p => exact g
  | cp x y =>
    refine ⟨g.initFrozen, g.fieldsMine, ?_, ?_⟩
    · intro z o he
      simp only [exec, setEnv] at he
      split at he
      · exact g.envMine y o he
      · exact g.envMine z o he
    · intro z o p o' he hd hs
      simp only [exec, setEnv] at he hd
      split at he
      · rename_i hz; subst hz
        exact hst p (g.modes y o p o' he hd hs)
      · exact g.modes z o p o' he hd hs
  | proj x y l =>
    refine ⟨g.initFrozen, g.fieldsMine, ?_, ?_⟩
    · intro z o he
      simp only [exec, setEnv] at he
      split at he
      · simp only [projOf] at he
        cases hy : s.env y with
        | none => rw [hy] at he; simp at he
        | some oy =>
          rw [hy] at he
          exact g.fieldsMine oy l o (g.envMine y oy hy) he
      · exact g.envMine z o he
    · intro z o p o' he hd hs
      simp only [exec, setEnv] at he hd
      split at he
      · rename_i hz; subst hz
        simp only [projOf] at he
        cases hy : s.env y with
        | none => rw [hy] at he; simp at he
        | some oy =>
          rw [hy] at he
          apply hst p
          apply g.modes y oy (l :: p) o' hy _ hs
          simp only [deref, he]
          exact hd
      · exact g.modes z o p o' he hd hs
  | mk x fs =>
    have hnext : s.next ≤ s.next + 1 := Nat.le_succ _
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro n l
      simp only [exec]
      have : Obj.init n ≠ Obj.fresh P s.next := by intro h; cases h
      simp only [this, ite_false]
      exact g.initFrozen n l
    · intro o l o' hm hf
      simp only [exec] at hf
      split at hf
      · simp only [mkFields] at hf
        cases hfy : findField l fs with
        | none => rw [hfy] at hf; simp at hf
        | some y =>
          rw [hfy] at hf
          exact mine_mono h0 caps hnext (g.envMine y o' hf)
      · rename_i hne
        have hm' : Mine h0 caps P s o := by
          rcases hm with ⟨i, rfl, hi⟩ | h
          · refine Or.inl ⟨i, rfl, ?_⟩
            have : i ≠ s.next := by intro he; apply hne; rw [he]
            simp only [exec] at hi
            omega
          · exact Or.inr h
        exact mine_mono h0 caps hnext (g.fieldsMine o l o' hm' hf)
    · intro z o he
      simp only [exec, setEnv] at he
      split at he
      · simp at he; subst he
        exact Or.inl ⟨s.next, rfl, by simp [exec]⟩
      · exact mine_mono h0 caps hnext (g.envMine z o he)
    · intro z o p o' he hd hs
      simp only [exec, setEnv] at he hd
      split at he
      · rename_i hz; subst hz
        simp at he; subst he
        cases p with
        | nil =>
          simp [deref] at hd
          subst hd
          exact absurd hs (fresh_not_shared h0 caps hh P s.next)
        | cons l p =>
          simp only [deref, ite_true] at hd
          simp only [mkFields] at hd
          cases hfy : findField l fs with
          | none => simp [hfy] at hd
          | some y =>
            cases hy : s.env y with
            | none => simp [hfy, hy] at hd
            | some oy =>
              simp only [hfy, hy] at hd
              rw [deref_frame h0 caps g _ p oy (g.envMine y oy hy)] at hd
              exact hst l y p (findField_mem hfy) (g.modes y oy p o' hy hd hs)
      · rw [deref_frame h0 caps g _ p o (g.envMine z o he)] at hd
        exact g.modes z o p o' he hd hs

/-- Every event of a run is on an object the party can reach, and is
assigned an atomic mode when that object is shared. -/
theorem run_events (hh : HeapOk h0 caps) {P : PartyId} {M : Modes} :
    ∀ (code : List Stmt) (s : St), Good h0 caps P M s → Closed M code →
      ∀ e ∈ run P s code,
        Prov h0 caps P e.2.2 ∧ (Shared h0 caps e.2.2 → M e.1 e.2.1)
  | [], _, _, _, e, he => by simp [run] at he
  | st :: rest, s, g, hc, e, he => by
    simp only [run, List.mem_append] at he
    rcases he with he | he
    · cases st with
      | mk _ _ => simp [exec] at he
      | cp _ _ => simp [exec] at he
      | proj _ _ _ => simp [exec] at he
      | touch x p =>
        simp only [exec, touchEvents] at he
        cases hx : s.env x with
        | none => simp [hx] at he
        | some o =>
          cases hd : deref s.heap o p with
          | none => simp [hx, hd] at he
          | some o' =>
            simp only [hx, hd, List.mem_singleton] at he
            subst he
            exact ⟨mine_prov h0 caps (deref_mine h0 caps g p o o' (g.envMine x o hx) hd),
                   fun hs => g.modes x o p o' hx hd hs⟩
    · exact run_events hh rest (exec P s st).1
        (good_exec h0 caps hh g hc.1 st (closed_head hc)) (closed_tail hc) e he

/-- **Theorem A.** In one window, when two different parties touch the same
object, that object is shared, and both touches are assigned atomic modes. -/
theorem window_sound (hh : HeapOk h0 caps) (M : Modes)
    (P Q : PartyId) (hPQ : P ≠ Q)
    (codeP codeQ : List Stmt) (hcP : Closed M codeP) (hcQ : Closed M codeQ)
    (envP envQ : Env)
    (henvP : ∀ x c, envP x = some c → caps P c)
    (henvQ : ∀ x c, envQ x = some c → caps Q c)
    (hseedP : ∀ x c, envP x = some c →
      (∃ p o, deref h0 c p = some o ∧ Shared h0 caps o) → M x [])
    (hseedQ : ∀ x c, envQ x = some c →
      (∃ p o, deref h0 c p = some o ∧ Shared h0 caps o) → M x [])
    (e1 e2 : Event)
    (h1 : e1 ∈ run P { heap := h0, env := envP, next := 0 } codeP)
    (h2 : e2 ∈ run Q { heap := h0, env := envQ, next := 0 } codeQ)
    (hsame : e1.2.2 = e2.2.2) :
    Shared h0 caps e1.2.2 ∧ M e1.1 e1.2.1 ∧ M e2.1 e2.2.1 := by
  have r1 := run_events h0 caps hh codeP _
    (good_init h0 caps hh P M hcP.1 envP henvP hseedP) hcP e1 h1
  have r2 := run_events h0 caps hh codeQ _
    (good_init h0 caps hh Q M hcQ.1 envQ henvQ hseedQ) hcQ e2 h2
  have hshared : Shared h0 caps e1.2.2 := by
    rcases r1.1 with ⟨i, hi⟩ | ⟨n, hn, hr1⟩ <;> rcases r2.1 with ⟨j, hj⟩ | ⟨m, hm, hr2⟩
    · rw [hsame, hj] at hi
      injection hi with hq _
      exact absurd hq.symm hPQ
    · rw [hsame, hm] at hi; cases hi
    · rw [hsame, hj] at hn; cases hn
    · exact ⟨P, Q, hPQ, hr1, hsame ▸ hr2⟩
  exact ⟨hshared, r1.2 hshared, r2.2 (hsame ▸ hshared)⟩

/-- Theorem A for the least solution: seeding the captured parameters and
closing forward is enough. -/
theorem window_sound_least (hh : HeapOk h0 caps)
    (P Q : PartyId) (hPQ : P ≠ Q)
    (codeP codeQ : List Stmt) (seeds : Local × Path → Prop)
    (envP envQ : Env)
    (henvP : ∀ x c, envP x = some c → caps P c)
    (henvQ : ∀ x c, envQ x = some c → caps Q c)
    (hseedP : ∀ x c, envP x = some c →
      (∃ p o, deref h0 c p = some o ∧ Shared h0 caps o) → seeds (x, []))
    (hseedQ : ∀ x c, envQ x = some c →
      (∃ p o, deref h0 c p = some o ∧ Shared h0 caps o) → seeds (x, []))
    (e1 e2 : Event)
    (h1 : e1 ∈ run P { heap := h0, env := envP, next := 0 } codeP)
    (h2 : e2 ∈ run Q { heap := h0, env := envQ, next := 0 } codeQ)
    (hsame : e1.2.2 = e2.2.2) :
    Shared h0 caps e1.2.2 ∧
      Reach (CodeEdge (codeP ++ codeQ)) seeds (e1.1, e1.2.1) ∧
      Reach (CodeEdge (codeP ++ codeQ)) seeds (e2.1, e2.2.1) := by
  have hc := reach_closed (codeP ++ codeQ) seeds
  have hcP : Closed (fun x p => Reach (CodeEdge (codeP ++ codeQ)) seeds (x, p)) codeP :=
    ⟨hc.1, fun st hs => hc.2 st (List.mem_append.mpr (Or.inl hs))⟩
  have hcQ : Closed (fun x p => Reach (CodeEdge (codeP ++ codeQ)) seeds (x, p)) codeQ :=
    ⟨hc.1, fun st hs => hc.2 st (List.mem_append.mpr (Or.inr hs))⟩
  exact window_sound h0 caps hh _ P Q hPQ codeP codeQ hcP hcQ envP envQ henvP henvQ
    (fun x c he hx => .seed (hseedP x c he hx))
    (fun x c he hx => .seed (hseedQ x c he hx)) e1 e2 h1 h2 hsame

end Window

end RcAtomicity
