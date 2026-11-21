import RowsEvidence.Syntax
/-! Print model terms as Roc source. -/
namespace RE

partial def G.roc : G → String
  | .u64 => "U64"
  | .str => "Str"
  | .nm => "Nm"
  | .tags ts =>
      let xs := ts.map fun (c, ps) => if ps.isEmpty then c else s!"{c}({", ".intercalate (ps.map G.roc)})"
      s!"[{", ".intercalate xs}]"
  | .record fs => "{ " ++ ", ".intercalate (fs.map fun (f, t) => s!"{f} : {t.roc}") ++ " }"
  | .try_ ok err => s!"Try({ok.roc}, {err.roc})"
  | .fn as r =>
      let arg (t : G) := match t with | .fn .. => s!"({t.roc})" | _ => t.roc
      s!"{", ".intercalate (as.map arg)} -> {r.roc}"
  | .list t => s!"List({t.roc})"
  | .box t => s!"Box({t.roc})"

def ind (n : Nat) : String := String.ofList (List.replicate (4 * n) ' ')

partial def Pat.roc : Pat → String
  | .wild => "_"
  | .bind x => x
  | .tag c [] => c
  | .tag c ps => s!"{c}({", ".intercalate (ps.map Pat.roc)})"

/-- `atomic` expressions need no parentheses as a callee/receiver. -/
def Expr.atomic : Expr → Bool
  | .num _ | .str _ | .var _ | .tag _ _ | .record _ | .call _ _ | .field _ _ | .meth _ _ _ | .list _ => true
  | _ => false

mutual
partial def Expr.roc (e : Expr) (d : Nat := 0) : String :=
  match e with
  | .num n => s!"{n}.U64"
  | .str s => "\"" ++ s ++ "\""
  | .var x => x
  | .add a k => s!"({a.roc d} + {k})"
  | .sub a k => s!"({a.roc d} - {k})"
  | .list es => s!"[{", ".intercalate (es.map (·.roc d))}]"
  | .tag c [] => c
  | .tag c as => s!"{c}({", ".intercalate (as.map (·.roc d))})"
  | .record fs => "{ " ++ ", ".intercalate (fs.map fun (f, v) => s!"{f}: {v.roc d}") ++ " }"
  | .field r f => s!"{paren r d}.{f}"
  | .call (.var f) as => s!"{f}({", ".intercalate (as.map (·.roc d))})"
  | .call f as => s!"({f.roc d})({", ".intercalate (as.map (·.roc d))})"
  | .ifEq a k t f => s!"(if {a.roc d} == {k} {branch t d} else {branch f d})"
  | .match_ s arms =>
      let lines := arms.map fun (p, b) => s!"{ind (d+1)}{p.roc} => {b.roc (d+1)}"
      s!"match {paren s d} \{\n{"\n".intercalate lines}\n{ind d}}"
  | .let_ .. | .tryLet .. => block e d
  | .meth r m as => s!"{paren r d}.{m}({", ".intercalate (as.map (·.roc d))})"
  | .lam ps b => s!"(|{", ".intercalate ps}| {b.roc d})"

partial def paren (e : Expr) (d : Nat) : String :=
  if e.atomic then e.roc d else s!"({e.roc d})"

partial def branch (e : Expr) (d : Nat) : String :=
  match e with
  | .let_ .. | .tryLet .. => block e d
  | _ => if e.atomic then e.roc d else s!"({e.roc d})"

/-- A block `{ stmts; value }`. -/
partial def block (e : Expr) (d : Nat) : String :=
  let rec stmts (e : Expr) (acc : List String) : List String × Expr :=
    match e with
    | .let_ x v b => stmts b (acc ++ [s!"{ind (d+1)}{x} = {v.roc (d+1)}"])
    | .tryLet x v b => stmts b (acc ++ [s!"{ind (d+1)}{x} = {paren v (d+1)}?"])
    | other => (acc, other)
  let (ss, v) := stmts e []
  "{\n" ++ "\n".intercalate (ss ++ [s!"{ind (d+1)}{v.roc (d+1)}"]) ++ s!"\n{ind d}}"
end

def Def.roc (df : Def) : String :=
  let ann := match df.annText, df.ann with
    | some t, _ => s!"{df.name} : {t}\n"
    | none, some t => s!"{df.name} : {t.roc}\n"
    | none, none => ""
  s!"{ann}{df.name} = {df.body.roc 0}\n"

/-- The fixed nominal every program may use (a user nominal with a backing and two methods). -/
def nmDecl : String :=
"Nm := [Nm(U64)].{
    to_str : Nm -> Str
    to_str = |self| match self {
        Nm(x) => \"Nm\".concat(x.to_str())
    }
    bump : Nm -> Nm
    bump = |self| match self {
        Nm(x) => Nm(x + 1)
    }
    label : Nm -> Str
    label = nm_label
}

nm_label = |self| match self {
    Nm(x) => \"L\".concat(x.to_str())
}

Box(a) := [Box(a)].{
    unbox : Box(a) -> a
    unbox = |self| match self {
        Box(x) => x
    }
}

mk_box : a -> Box(a)
mk_box = |x| Box(x)
"

def Program.roc (p : Program) (modName : String := "H") : String :=
  let defs := "\n".intercalate (p.defs.map Def.roc)
  let exps := "\n".intercalate (p.expects.map fun (l, r) => s!"expect {l.roc 0} == {paren r 0}")
  let raws := "\n\n".intercalate (p.rawExpects.map fun b => s!"expect {b}")
  s!"{modName} := [{modName}].\{}\n\n{nmDecl}\n{defs}\n{exps}\n\n{raws}\n"

end RE
