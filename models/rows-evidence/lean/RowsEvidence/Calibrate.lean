import RowsEvidence.Replay
import RowsEvidence.Gen
/-!
  Calibration: known bugs written as model terms, each with the verdict the model must give.

  For a fixed bug, the `Cfg` toggle that restores the pre-fix rule must make the matching check
  fail, and the current rules must pass. For a bug where the compiler rejected a program that
  design.md accepts, the model must accept it with a clean replay. `./run.sh selftest` runs this.
-/
namespace RE

def mkNmDef : Def := { name := "mk_nm", ann := some (.fn [.u64] .nm), body := .lam ["n"] (.tag "Nm" [.var "n"]) }

/-- #12625: `fail = |tag| { Err(Early)?; Err(tag) }`, called at a closed `[Missing]`. -/
def cal12625 : Program := {
  defs := [{ name := "fail", ann := none,
             body := .lam ["tag"] (.tryLet "_x" (.tag "Err" [.tag "Early" []]) (.tag "Err" [.var "tag"])) }],
  expects := [(.call (.var "fail") [.tag "Missing" []], .tag "Err" [.tag "Early" []])] }

/-- #12552 (shape): a point-free alias of a function whose scheme carries a where-requirement. -/
def cal12552 : Program := {
  defs := [mkNmDef,
           { name := "show", ann := none, body := .lam ["x"] (.meth (.var "x") "to_str" []) },
           { name := "show2", ann := none, body := .var "show" }],
  expects := [(.call (.var "show2") [.call (.var "mk_nm") [.num 1]], .str "Nm1")] }

/-- #13236: an unmatchable arm's payload shares the parameter's type. -/
def cal13236 : Program := {
  defs := [{ name := "f", ann := none, body := .lam ["n"] (.match_ (.tag "Oops" [])
      [(.tag "Oops" [], .var "n"), (.tag "Zero" [.bind "p"], .var "p")]) }],
  expects := [(.call (.var "f") [.num 1], .num 1)] }

/-- #13237: `f : U64 -> Try(U64, _)` with two `Ok` branches, used at a wider row. -/
def cal13237 : Program := {
  defs := [{ name := "f", ann := some (.fn [.u64] (.try_ .u64 (.tags []))),
             annText := some "U64 -> Try(U64, _)", annMode := 1,
             body := .lam ["n"] (.ifEq (.var "n") 0 (.tag "Ok" [.num 0]) (.tag "Ok" [.var "n"])) },
           { name := "x", ann := some (.try_ .u64 (.tags [("B", [])])),
             body := .call (.var "f") [.num 1] }],
  expects := [(.var "x", .tag "Ok" [.num 1])] }

/-- What a calibration case must produce: a clean replay, or a violation whose text contains `needle`. -/
inductive Want where
  | clean
  | viol (needle : String)

/-- Run one case. Returns the report line and whether the verdict is the wanted one. -/
def runCal (name : String) (p : Program) (cfg : Cfg) (want : Want) : String × Bool :=
  match checkProgram p with
  | .error e => (s!"{name}: model checker rejects ({e})", false)
  | .ok ck =>
    let vs := (replayProgram ck cfg).map Viol.show
    let got := if vs.isEmpty then "replay clean" else "; ".intercalate vs
    let ok := match want with
      | .clean => vs.isEmpty
      | .viol needle => vs.any (fun v => (v.splitOn needle).length > 1)
    (s!"{name}: {got}", ok)

def calibrationCases : List (String × Bool) :=
  let codecNow := codecCheck false
  let codecPre := codecCheck true
  [ runCal "#12625 current rules" cal12625 {} .clean,
    runCal "#12625 pre-fix rule (exact construction payload)" cal12625 { exactConstructionPayload := true }
      (.viol "widened a closed tag union with Early"),
    runCal "#12552 current rules" cal12552 {} .clean,
    runCal "#12552 pre-fix rule (alias drops evidence)" cal12552 { aliasDropsEvidence := true }
      (.viol "published unreachable, reached at inhabited Nm"),
    (s!"#12229 current rules: {codecNow.length} codec shapes with calls ≠ Req", codecNow.isEmpty),
    (s!"#12229 pre-fix rule: {codecPre.length} codec shapes with calls ≠ Req, first {(codecPre.head?.map G.roc).getD "-"}",
      !codecPre.isEmpty),
    runCal "#13236 (design.md accepts)" cal13236 {} .clean,
    runCal "#13237 (design.md accepts)" cal13237 {} .clean ]

end RE
