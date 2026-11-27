import RowsEvidence.Syntax
import RowsEvidence.Print
/-! Reference big-step semantics: the oracle for `expect` values (miscompile detection). -/
namespace RE

inductive V where
  | num (n : Nat)
  | str (s : String)
  | tag (c : String) (args : List V)
  | record (fs : List (String × V))
  | list (vs : List V)
  | clo (ps : List String) (b : Expr) (env : List (String × V))
  deriving Repr, Inhabited

/-- Early exit: `?` returning `Err(e)` from the enclosing function, or a stuck evaluation. -/
inductive Exit where
  | ret (v : V)
  | stuck (why : String)
  deriving Repr, Inhabited

abbrev EM := ExceptT Exit (StateM Nat)

def tick : EM Unit := do
  let f ← get
  if f == 0 then throw (.stuck "fuel")
  set (f - 1)

partial def V.toExpr : V → Option Expr
  | .num n => some (.num n)
  | .str s => some (.str s)
  | .tag c as => do
      let es ← as.mapM V.toExpr
      return .tag c es
  | .record fs => do
      let es ← fs.mapM fun (f, v) => do return (f, ← v.toExpr)
      return .record es
  | .list vs => do return .list (← vs.mapM V.toExpr)
  | .clo .. => none

partial def V.beq : V → V → Bool
  | .num a, .num b => a == b
  | .str a, .str b => a == b
  | .tag c as, .tag d bs => c == d && as.length == bs.length && (as.zip bs).all fun (x, y) => x.beq y
  | .record fs, .record gs => fs.length == gs.length && fs.all fun (f, v) => (gs.find? (·.1 == f)).any (·.2.beq v)
  | .list as, .list bs => as.length == bs.length && (as.zip bs).all fun (x, y) => x.beq y
  | _, _ => false

partial def matchPat (p : Pat) (v : V) : Option (List (String × V)) :=
  match p, v with
  | .wild, _ => some []
  | .bind x, v => some [(x, v)]
  | .tag c ps, .tag d vs =>
      if c == d && ps.length == vs.length then
        (ps.zip vs).foldlM (fun acc (p, v) => do return acc ++ (← matchPat p v)) []
      else none
  | _, _ => none

mutual
partial def eval (globals : List Def) (env : List (String × V)) (e : Expr) : EM V := do
  tick
  match e with
  | .num n => return .num n
  | .str s => return .str s
  | .var x =>
      match env.find? (·.1 == x) with
      | some (_, v) => return v
      | none =>
        match globals.find? (·.name == x) with
        | some d => eval globals [] d.body
        | none =>
          if x == "mk_box" then return .clo ["x"] (.tag "Box" [.var "x"]) []
          else throw (.stuck s!"unbound {x}")
  | .add a k => do
      match ← eval globals env a with
      | .num n => return .num (n + k)
      | _ => throw (.stuck "add")
  | .sub a k => do
      match ← eval globals env a with
      | .num n => if n < k then throw (.stuck "underflow") else return .num (n - k)
      | _ => throw (.stuck "sub")
  | .list es => return .list (← es.mapM (eval globals env))
  | .tag c as => return .tag c (← as.mapM (eval globals env))
  | .record fs => return .record (← fs.mapM fun (f, x) => do return (f, ← eval globals env x))
  | .field r f => do
      match ← eval globals env r with
      | .record fs => match fs.find? (·.1 == f) with
          | some (_, v) => return v
          | none => throw (.stuck s!"field {f}")
      | _ => throw (.stuck "field of non-record")
  | .call f as => do
      let fv ← eval globals env f
      let avs ← as.mapM (eval globals env)
      apply globals fv avs
  | .ifEq a k t f => do
      match ← eval globals env a with
      | .num n => if n == k then eval globals env t else eval globals env f
      | _ => throw (.stuck "if")
  | .match_ s arms => do
      let sv ← eval globals env s
      for (p, b) in arms do
        if let some bs := matchPat p sv then
          return ← eval globals (bs ++ env) b
      throw (.stuck "no arm")
  | .let_ x v b => do
      let vv ← eval globals env v
      eval globals ((x, vv) :: env) b
  | .tryLet x v b => do
      match ← eval globals env v with
      | .tag "Ok" [ok] => eval globals ((x, ok) :: env) b
      | .tag "Err" [err] => throw (.ret (.tag "Err" [err]))
      | _ => throw (.stuck "? on non-Try")
  | .meth r m as => do
      let rv ← eval globals env r
      let avs ← as.mapM (eval globals env)
      match rv, m, avs with
      | .num n, "to_str", [] => return .str (toString n)
      | .str a, "concat", [.str b] => return .str (a ++ b)
      | .tag "Nm" [.num n], "to_str", [] => return .str s!"Nm{n}"
      | .tag "Nm" [.num n], "bump", [] => return .tag "Nm" [.num (n + 1)]
      | .tag "Nm" [.num n], "label", [] => return .str s!"L{n}"
      | .tag "Box" [v], "unbox", [] => return v
      | _, _, _ => throw (.stuck s!"method {m}")
  | .lam ps b => return .clo ps b env

partial def apply (globals : List Def) (fv : V) (avs : List V) : EM V := do
  match fv with
  | .clo ps b cenv =>
      if ps.length != avs.length then throw (.stuck "arity")
      tryCatch (eval globals (ps.zip avs ++ cenv) b) fun
        | .ret v => return v
        | e => throw e
  | _ => throw (.stuck "apply non-function")
end

def evalTop (p : Program) (e : Expr) (fuel : Nat := 100000) : Except String V :=
  match ((eval p.defs [] e).run.run fuel).1 with
  | Except.ok v => .ok v
  | Except.error (Exit.ret v) => .ok v   -- `?` at expect level: Roc reports it as a failed expect; we never generate it
  | Except.error (Exit.stuck w) => .error w

end RE
