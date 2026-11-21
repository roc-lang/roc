import RowsEvidence
open RE

def usage : String :=
  "usage: rows-evidence gen SEED COUNT OUTDIR | model SEED COUNT | calibrate | lifted FILE..."

def main (args : List String) : IO UInt32 := do
  match args with
  | ["gen", seed, count, out] =>
      -- write each generated program that has at least one `expect` to OUTDIR/g<seed>/H.roc
      let s0 := seed.toNat!
      for i in [0:count.toNat!] do
        let p := genProgram (s0 + i)
        if p.expects.isEmpty then continue
        let dir := s!"{out}/g{s0 + i}"
        IO.FS.createDirAll dir
        IO.FS.writeFile s!"{dir}/H.roc" p.roc
      return 0
  | ["model", seed, count] =>
      -- the model's verdict on each generated program: ACCEPT/REJECT and replay violations
      let s0 := seed.toNat!
      for i in [0:count.toNat!] do
        let p := genProgram (s0 + i)
        if p.expects.isEmpty then continue
        match checkProgram p with
        | .error e => IO.println s!"g{s0 + i}\tREJECT\t{e}"
        | .ok ck =>
          let vs := replayProgram ck
          IO.println s!"g{s0 + i}\tACCEPT\t{vs.length}\t{"; ".intercalate (vs.map Viol.show)}"
      return 0
  | ["calibrate"] =>
      -- known bugs as model terms: each must get its wanted verdict (Calibrate.lean)
      let mut bad := 0
      for (line, ok) in calibrationCases do
        IO.println s!"{if ok then "ok  " else "FAIL"} {line}"
        unless ok do bad := bad + 1
      return (if bad == 0 then 0 else 2)
  | "lifted" :: files@(_ :: _) =>
      -- parse each Lifted IR export and run the independent typing check
      let mut bad := 0
      for f in files do
        let text ← IO.FS.readFile f
        match Lifted.parse text with
        | .error e => bad := bad + 1; IO.println s!"{f}\tPARSE_ERROR\t{e}"
        | .ok pg =>
          let vs := Lifted.checkProgram pg
          if vs.isEmpty then IO.println s!"{f}\tOK\t{Lifted.summary pg}"
          else
            bad := bad + 1
            IO.println s!"{f}\tVIOL\t{vs.size}\t{Lifted.summary pg}"
            for v in vs.toList.take 20 do IO.println s!"  {v.rule}\tf{v.fn}\t{v.site}\t{v.msg}"
      return (if bad == 0 then 0 else 2)
  | _ => IO.eprintln usage; return 1
