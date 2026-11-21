/-
  RocArc.Syntax: the first-order statement IR the model runs.

  It mirrors final (post-ARC) Roc LIR as printed by `src/lir/debug_print.zig`
  (`ROC_LIR_DUMP`). Locals are procedure-wide mutable slots, exactly as in LIR:
  join parameters are ordinary locals that the jumping path assigns before
  `jump`, and a join's body runs in the same frame as its remainder.
-/
namespace RocArc

abbrev Local := Nat
abbrev ProcId := Nat
abbrev JoinId := Nat

/-- Bit `i` of mask `m`. -/
def bit (m i : Nat) : Bool := Nat.testBit m i

/-- An `RcEffect` row (`src/base/LowLevel.zig` `LowLevel.RcEffect`), plus the
    two per-op facts ARC also reads: `representationArgs` and `deadLenderArg`.
    The model's generated table (`OpTable.lean`) comes from the compiler source,
    so the rows are axioms shared with the compiler, not re-derived. -/
structure RcEffect where
  mayAllocate : Bool := false
  mayRetainOrRelease : Bool := false
  uniqCheck : Nat := 0
  consume : Nat := 0
  aliasConsumed : Nat := 0
  retain : Nat := 0
  retainResult : Bool := false
  borrowsArgs : Nat := 0
  sharesArgs : Nat := 0
  resultUnique : Bool := false
  reprArgs : Nat := 0
  deadLender : Option Nat := none
  deriving Repr, Inhabited, BEq

/-- Coarse layout kinds; the dump prints `kind#idx` or a sentinel name. -/
inductive LayoutKind where
  | scalar | str | list | box | struct_ | tagUnion | erased | closure | zst | ptr | dyn | other
  deriving Repr, Inhabited, BEq, DecidableEq

structure Ty where
  kind : LayoutKind
  idx : Option Nat := none     -- layout index when printed as `kind#idx`
  text : String := ""
  deriving Repr, Inhabited, BEq

inductive RefKind where
  | local_
  | discriminant
  | field (i : Nat)
  | tagPayload (variant i : Nat)
  | tagPayloadStruct (variant : Nat)
  | listReinterpret
  | nominal
  deriving Repr, Inhabited, BEq

inductive Lit where
  | int (n : Int)
  | float               -- f32/f64/dec: value irrelevant to RC
  | str                 -- a string literal: static (immortal) or small
  | staticData (id : Nat)
  | bytes
  | procRef (p : ProcId)
  deriving Repr, Inhabited, BEq

inductive Expr where
  | ref (k : RefKind) (src : Local) (take : Bool)
  | lit (l : Lit) (len : Option Nat)
  | call (p : ProcId) (args : List Local)
  | lowLevel (op : String) (args : List Local) (unique : Nat)
  | list (elems : List Local)
  | struct_ (fields : List Local)
  | tag (variant disc : Nat) (payload : Option Local)
  | unsupported (text : String)
  deriving Repr, Inhabited

inductive Stmt where
  | assign (x : Local) (ty : Ty) (e : Expr) (k : Stmt)
  | set (x y : Local) (k : Stmt)
  | inc (x : Local) (n : Nat) (k : Stmt)
  | dec (x : Local) (k : Stmt)
  | free (x : Local) (k : Stmt)
  | decIfInit (cond : Local) (mask : Nat) (x : Local) (k : Stmt)
  | initUninit (x : Local) (k : Stmt)
  | use (x : Local) (k : Stmt)                -- `debug` / `expect`: reads a local
  | switch (x : Local) (cases : List (Int × Stmt)) (dflt : Stmt) (cont : Option Stmt)
  | switchInit (cond : Local) (mask : Nat) (payload : Local) (initB uninitB : Stmt)
  | join (j : JoinId) (params : List Local) (remainder body : Stmt)
  | jump (j : JoinId)
  | ret (x : Local)
  | crash (msg : Option Local)   -- `crash lN` / `expect_err lN` consume the message
  | runtimeError
  | unsupported (text : String)
  deriving Repr, Inhabited

structure Proc where
  id : ProcId
  name : String
  params : List (Local × Ty)
  retTy : Ty
  borrowedMask : Nat        -- header `borrowed=`: bit i ⇒ param i is borrowed
  retBorrowed : Bool
  retUnique : Bool
  body : Stmt
  rcHelperLayouts : List Nat := []   -- layout ids named by this proc's RC helpers
  deriving Inhabited

structure Program where
  procs : List Proc
  deriving Inhabited

def Program.find? (P : Program) (p : ProcId) : Option Proc := P.procs.find? (·.id == p)

/-- Whether values of this layout may hold a refcounted allocation. Struct and
    tag layouts are decided by `rcLayouts` (the layout ids some RC helper in the
    dump names), because the dump does not print a layout's fields. -/
def Ty.mayBeRc (t : Ty) (rcLayouts : List Nat) : Bool :=
  match t.kind with
  | .str | .list | .box | .erased => true
  | .struct_ | .tagUnion | .closure => match t.idx with
    | some i => rcLayouts.contains i
    | none => false
  | _ => false

end RocArc

namespace RocArc
/-- Number of explicit RC statements in a body. A program with none is
    ownership-neutral LIR (no ARC was run on it), which neither judgment applies to. -/
partial def Stmt.rcCount : Stmt → Nat
  | .assign _ _ _ k | .set _ _ k | .initUninit _ k | .use _ k => k.rcCount
  | .inc _ _ k | .dec _ k | .free _ k | .decIfInit _ _ _ k => 1 + k.rcCount
  | .switch _ cs d c =>
    (cs.map fun (_, b) => b.rcCount).sum + d.rcCount + (match c with | some c => c.rcCount | none => 0)
  | .switchInit _ _ _ a b => a.rcCount + b.rcCount
  | .join _ _ r b => r.rcCount + b.rcCount
  | _ => 0

def Program.isOwnershipNeutral (P : Program) : Bool := P.procs.all (·.body.rcCount == 0)
end RocArc
