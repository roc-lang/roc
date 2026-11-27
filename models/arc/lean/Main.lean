import RocArc
open RocArc

def usage : String :=
  "usage:\n" ++
  "  rocarc oracle DUMP [--checked] [--steps N] [--paths N] [--forks N] [--verbose] [--trace]\n" ++
  "  rocarc equiv DUMP [--steps N] [--paths N] [--forks N]\n" ++
  "  rocarc certify DUMP [--verbose]"

def reportProgram (idx : Nat) (P : Program) (mode : Mode) (steps paths forks : Nat) (verbose : Bool) (trace : Bool := false) : IO Nat := do
  let mut bad := 0
  for root in P.procs do
    if !root.params.isEmpty then continue
    if root.name != "<unnamed>" then continue
    let r := runRoot P root mode steps paths forks trace
    let status := if r.violations.isEmpty then "ok" else "VIOLATION"
    if verbose || !r.violations.isEmpty || !r.stuck.isEmpty then
      IO.println s!"program {idx} root p{root.id}: {status} paths={r.paths} done={r.done} crashed={r.crashed} unsupported={r.unsupported.size} stuck={r.stuck.size} fuel={r.outOfFuel} stamps={r.stampsExecuted}{if r.truncated then " TRUNCATED" else ""}"
    let msgs := r.violations.toList.map Violation.render
    for v in msgs.eraseDups do
      IO.println s!"  {v}"
    for s in (r.stuck.toList.take 3) do IO.println s!"  stuck: {s}"
    if verbose then
      for s in (r.unsupported.toList.take 3) do IO.println s!"  unsupported: {s}"
      IO.println s!"  first path: {" > ".intercalate (r.firstTrail.take 60)}"
      if !r.violations.isEmpty then
        let vt := if trace then r.violationTrail else r.violationTrail.take 80
        IO.println s!"  first violation's path: {(if trace then "\n    " else " > ").intercalate vt}"
    if !r.violations.isEmpty then bad := bad + 1
  return bad

def main (args : List String) : IO UInt32 := do
  match args with
  | "oracle" :: file :: rest =>
    let text ← IO.FS.readFile file
    let mode := if rest.contains "--checked" then Mode.checked else Mode.stamped
    let verbose := rest.contains "--verbose"
    let num (flag : String) (d : Nat) : Nat :=
      match rest.dropWhile (· != flag) with
      | _ :: n :: _ => n.toNat?.getD d
      | _ => d
    match parseDump text with
    | .error e => IO.eprintln s!"parse error: {e}"; return 2
    | .ok P =>
      let progs := splitPrograms P
      IO.println s!"{file}: {P.procs.length} procs in {progs.length} programs"
      let mut bad := 0
      for (prog, i) in progs.zipIdx do
        if prog.isOwnershipNeutral then
          if verbose then IO.println s!"program {i}: ownership-neutral (no RC statements), skipped"
          continue
        bad := bad + (← reportProgram i prog mode (num "--steps" 20000) (num "--paths" 2000) (num "--forks" 8) (verbose || rest.contains "--trace") (rest.contains "--trace"))
      return (if bad == 0 then 0 else 1)
  | "equiv" :: file :: rest =>
    -- R-Equiv: run each root with in-place reuse (stamped) and in R-Pure mode;
    -- when both explore the same number of paths, compare results pathwise.
    let text ← IO.FS.readFile file
    let num (flag : String) (d : Nat) : Nat :=
      match rest.dropWhile (· != flag) with
      | _ :: n :: _ => n.toNat?.getD d
      | _ => d
    match parseDump text with
    | .error e => IO.eprintln s!"parse error: {e}"; return 2
    | .ok P =>
      let mut bad := 0
      let mut compared := 0
      for (prog, i) in (splitPrograms P).zipIdx do
        for root in prog.procs do
          if !root.params.isEmpty || root.name != "<unnamed>" then continue
          let a := runRoot prog root .stamped (num "--steps" 20000) (num "--paths" 200) (num "--forks" 4)
          let b := runRoot prog root .pure (num "--steps" 20000) (num "--paths" 200) (num "--forks" 4)
          if a.results.size == b.results.size && a.results.size > 0 && a.violations.isEmpty then
            compared := compared + 1
            for (x, y) in a.results.zip b.results do
              if !compatObs x.toList y.toList then
                bad := bad + 1
                IO.println s!"program {i} root p{root.id}: VALUE-MISMATCH in-place={x} pure={y}"
                break
          for v in (a.violations.toList.map Violation.render).eraseDups do
            IO.println s!"program {i} root p{root.id}: {v}"
      IO.println s!"{file}: compared {compared} roots, {bad} mismatches"
      return (if bad == 0 then 0 else 1)
  | "certify" :: file :: rest =>
    let text ← IO.FS.readFile file
    let verbose := rest.contains "--verbose"
    match parseDump text with
    | .error e => IO.eprintln s!"parse error: {e}"; return 2
    | .ok P =>
      let mut bad := 0
      for (prog, i) in (splitPrograms P).zipIdx do
        if prog.isOwnershipNeutral then
          IO.println s!"program {i}: ownership-neutral (no RC statements), skipped"
          continue
        let rcl := inferRcLayouts prog (prog.procs.flatMap (·.rcHelperLayouts))
        for p in prog.procs do
          match certifyProc prog rcl opMap p with
          | .ok () => if verbose then IO.println s!"program {i} {p.name}(p{p.id}): certified"
          | .error e =>
            if e.startsWith "[M-" then
              IO.println s!"program {i} {p.name}(p{p.id}): GAVE-UP {e}"
            else if e.startsWith "[C-Unsupported]" then
              -- a construct the certifier model does not interpret (G7): not a verdict
              IO.println s!"program {i} {p.name}(p{p.id}): UNSUPPORTED {e}"
            else
              bad := bad + 1
              IO.println s!"program {i} {p.name}(p{p.id}): REJECTED {e}"
      IO.println s!"{file}: {bad} procs rejected"
      return (if bad == 0 then 0 else 1)
  | _ => IO.eprintln usage; return 2
