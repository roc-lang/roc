/-
  RocArc.Parse — the abstraction function α: `ROC_LIR_DUMP` text → `Program`.

  The grammar is the one `src/lir/debug_print.zig` writes (see ../FORMAT.md).
  Anything the model does not interpret becomes `Stmt.unsupported` /
  `Expr.unsupported`, so α never guesses: a path that reaches one is reported
  as "not modeled", never as a pass.
-/
import RocArc.Syntax

namespace RocArc
namespace Parse

def trim (s : String) : String :=
  String.ofList ((s.toList.dropWhile Char.isWhitespace).reverse.dropWhile Char.isWhitespace).reverse
def trimRight (s : String) : String :=
  String.ofList ((s.toList.reverse.dropWhile Char.isWhitespace).reverse)
def dropR (s : String) (n : Nat) : String := String.ofList (s.toList.take (s.length - n))
def dropL (s : String) (n : Nat) : String := String.ofList (s.toList.drop n)

structure Line where
  indent : Nat
  text : String
  no : Nat
  deriving Inhabited

def indentOf (s : String) : Nat := (s.toList.takeWhile (· == ' ')).length / 2

/-- Split `a, b(c, d), e` at top-level commas. -/
def splitTop (s : String) : List String := Id.run do
  let mut out : Array String := #[]
  let mut cur : String := ""
  let mut depth : Int := 0
  for c in s.toList do
    if c == ',' && depth == 0 then
      out := out.push (trim cur); cur := ""
    else
      if c == '(' || c == '[' || c == '{' then depth := depth + 1
      if c == ')' || c == ']' || c == '}' then depth := depth - 1
      cur := cur.push c
  if (trim cur) != "" then out := out.push (trim cur)
  return out.toList

def natAfter (pfx s : String) : Option Nat :=
  if s.startsWith pfx then (s.drop pfx.length).toString.toNat? else none

def parseLocal (s : String) : Option Local := natAfter "l" (trim s)

def parseHex (s : String) : Nat :=
  let s := if s.startsWith "0x" then (s.drop 2).toString else s
  s.toList.foldl (fun acc c =>
    let d := if c.isDigit then c.toNat - '0'.toNat
      else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10 else 0
    acc * 16 + d) 0

def parseTy (s : String) : Ty :=
  let s := (trim s)
  if s.startsWith "dyn(" then { kind := .dyn, text := s } else
  match s.splitOn "#" with
  | [k, i] =>
    let kind := match k with
      | "list" | "list_of_zst" => LayoutKind.list
      | "box" | "box_of_zst" => .box
      | "struct_" => .struct_
      | "tag_union" => .tagUnion
      | "erased_callable" => .erased
      | "closure" => .closure
      | "zst" => .zst
      | "ptr" => .ptr
      | "dynamic" => .dyn
      | "scalar" => .scalar
      | _ => .other
    { kind, idx := i.toNat?, text := s }
  | _ =>
    let kind := match s with
      | "str" => LayoutKind.str
      | "zst" => .zst
      | _ => .scalar   -- bool, u8..i128, f32, f64, dec, opaque_ptr
    { kind, text := s }

/-- `l5:list#25` → (5, ty). -/
def parseTypedLocal (s : String) : Option (Local × Ty) :=
  match s.splitOn ":" with
  | l :: rest => do
    let x ← parseLocal l
    pure (x, parseTy (":".intercalate rest))
  | _ => none

/-- Locals in `(l1, l2)` or `l1, l2`. -/
def parseLocals (s : String) : Option (List Local) :=
  if (trim s) == "" then some [] else (splitTop s).mapM parseLocal

/-- Text between the first `open` and its matching `close`, and the rest. -/
def bracketed (s : String) (op cl : Char) : Option (String × String) := Id.run do
  let cs := s.toList
  match cs.findIdx? (· == op) with
  | none => return none
  | some start =>
    let mut depth := 0
    let mut i := start
    for c in cs.drop start do
      if c == op then depth := depth + 1
      if c == cl then
        depth := depth - 1
        if depth == 0 then
          let inner := String.ofList ((cs.drop (start + 1)).take (i - start - 1))
          let rest := String.ofList (cs.drop (i + 1))
          return some (inner, rest)
      i := i + 1
    return none

def words (s : String) : List String := (s.splitOn " ").filter (· != "")

def parseRef (rest : String) : Expr :=
  let ws := words rest
  let take := ws.contains "take"
  match ws with
  | "ref.local" :: l :: _ => match parseLocal l with
    | some x => .ref .local_ x take | none => .unsupported rest
  | "ref.discriminant" :: l :: _ => match parseLocal l with
    | some x => .ref .discriminant x take | none => .unsupported rest
  | "ref.list_reinterpret" :: l :: _ => match parseLocal l with
    | some x => .ref .listReinterpret x take | none => .unsupported rest
  | "ref.nominal" :: l :: _ => match parseLocal l with
    | some x => .ref .nominal x take | none => .unsupported rest
  | "ref.field" :: li :: _ =>       -- l3[1]
    match li.splitOn "[" with
    | [l, i] => match parseLocal l, (dropR i 1).toNat? with
      | some x, some n => .ref (.field n) x take
      | _, _ => .unsupported rest
    | _ => .unsupported rest
  | "ref.tag_payload" :: l :: vi :: _ =>   -- l3 v1[0]
    match vi.splitOn "[" with
    | [v, i] => match parseLocal l, natAfter "v" v, (dropR i 1).toNat? with
      | some x, some vv, some n => .ref (.tagPayload vv n) x take
      | _, _, _ => .unsupported rest
    | _ => .unsupported rest
  | "ref.tag_payload_struct" :: l :: v :: _ =>
    match parseLocal l, natAfter "v" v with
    | some x, some vv => .ref (.tagPayloadStruct vv) x take
    | _, _ => .unsupported rest
  | _ => .unsupported rest

def parseLiteral (rest : String) : Expr :=
  let ws := words rest
  let len := (ws.filterMap (natAfter "len=")).head?
  match ws with
  | "literal" :: "str" :: _ => .lit .str len
  | "literal" :: "bytes" :: _ => .lit .bytes len
  | "literal" :: "static_data" :: s :: _ => match natAfter "s" s with
    | some n => .lit (.staticData n) len | none => .unsupported rest
  | "literal" :: "proc_ref" :: p :: _ => match natAfter "p" p with
    | some n => .lit (.procRef n) len | none => .unsupported rest
  | "literal" :: "f64" :: _ => .lit .float len
  | "literal" :: "f32" :: _ => .lit .float len
  | "literal" :: "dec" :: _ => .lit .float len
  | "literal" :: n :: _ => match n.toInt? with
    | some i => .lit (.int i) len
    | none => .lit .float len
  | _ => .unsupported rest

def parseRhs (rhs : String) : Expr :=
  let rhs := (trim rhs)
  if rhs.startsWith "ref." then parseRef rhs
  else if rhs.startsWith "literal" then parseLiteral rhs
  else if rhs.startsWith "call p" then
    match bracketed rhs '(' ')' with
    | some (args, _) =>
      let head := (rhs.splitOn "(").head!
      match natAfter "call p" head, parseLocals args with
      | some p, some xs => .call p xs
      | _, _ => .unsupported rhs
    | none => .unsupported rhs
  else if rhs.startsWith "low_level " then
    match bracketed rhs '(' ')' with
    | some (args, after) =>
      let op := ((rhs.drop "low_level ".length).toString.splitOn "(").head!
      let uniq := ((words after).filterMap (fun w =>
        if w.startsWith "unique=" then some (parseHex (w.drop 7).toString) else none)).head?.getD 0
      match parseLocals args with
      | some xs => .lowLevel op xs uniq
      | none => .unsupported rhs
    | none => .unsupported rhs
  else if rhs.startsWith "list(" then
    match bracketed rhs '(' ')' with
    | some (args, _) => match parseLocals args with
      | some xs => .list xs | none => .unsupported rhs
    | none => .unsupported rhs
  else if rhs.startsWith "struct(" then
    match bracketed rhs '(' ')' with
    | some (args, _) => match parseLocals args with
      | some xs => .struct_ xs | none => .unsupported rhs
    | none => .unsupported rhs
  else if rhs.startsWith "tag v" then
    match words rhs with
    | "tag" :: v :: d :: rest =>
      let payload := match rest with
        | p :: _ => parseLocal (dropR (dropL p 1) 1)
        | [] => none
      match natAfter "v" v, natAfter "d" d with
      | some vv, some dd =>
        if rest.isEmpty || payload.isSome then .tag vv dd payload else .unsupported rhs
      | _, _ => .unsupported rhs
    | _ => .unsupported rhs
  else .unsupported rhs

structure St where
  lines : Array Line
  pos : Nat
  deriving Inhabited

abbrev P := StateT St (Except String)

def peek : P (Option Line) := do
  let s ← get
  return s.lines[s.pos]?

def advance : P Unit := modify fun s => { s with pos := s.pos + 1 }

def expectLabel (indent : Nat) (label : String) : P Unit := do
  match (← peek) with
  | some l =>
    if l.indent == indent && l.text == label then advance
    else throw s!"line {l.no}: expected `{label}` at indent {indent}, got `{l.text}`"
  | none => throw s!"expected `{label}`, got end of input"

/-- A field `key=value` among the words of a line. -/
def field (key : String) (ws : List String) : Option String :=
  (ws.filterMap (fun w => if w.startsWith (key ++ "=") then some (w.drop (key.length + 1)).toString else none)).head?

mutual
/-- A statement chain whose lines sit at `indent`. -/
partial def chain (indent : Nat) : P Stmt := do
  let some l ← peek | throw "unexpected end of input in a chain"
  if l.indent != indent then throw s!"line {l.no}: expected indent {indent}, got {l.indent}: `{l.text}`"
  advance
  let t := l.text
  let ws := words t
  match ws with
  | "join" :: j :: _ =>
    let some jid := natAfter "j" j | throw s!"line {l.no}: bad join"
    let some (ps, _) := bracketed t '[' ']' | throw s!"line {l.no}: bad join params"
    let some params := parseLocals ps | throw s!"line {l.no}: bad join params"
    expectLabel (indent + 1) "remainder:"
    let rem ← chain (indent + 2)
    expectLabel (indent + 1) "body:"
    let body ← chain (indent + 2)
    return .join jid params rem body
  | "switch" :: x :: _ =>
    let some xl := parseLocal x | throw s!"line {l.no}: bad switch"
    let mut cases : Array (Int × Stmt) := #[]
    repeat
      match (← peek) with
      | some c =>
        if c.indent == indent + 1 && c.text.startsWith "case " then
          advance
          let v := (dropR (dropL c.text 5) 1).toInt?.getD 0
          let b ← chain (indent + 2)
          cases := cases.push (v, b)
        else break
      | none => break
    expectLabel (indent + 1) "default:"
    let d ← chain (indent + 2)
    let cont ← do
      match (← peek) with
      | some c =>
        if c.indent == indent + 1 && c.text == "continuation:" then
          advance; pure (some (← chain (indent + 2)))
        else pure none
      | none => pure none
    return .switch xl cases.toList d cont
  | "switch_initialized_payload" :: _ =>
    let cond := (field "cond" ws).bind parseLocal
    let mask := parseHex ((field "mask" ws).getD "0")
    let payload := (field "payload" ws).bind parseLocal
    expectLabel (indent + 1) "initialized:"
    let a ← chain (indent + 2)
    expectLabel (indent + 1) "uninitialized:"
    let b ← chain (indent + 2)
    match cond, payload with
    | some c, some p => return .switchInit c mask p a b
    | _, _ => throw s!"line {l.no}: bad switch_initialized_payload"
  | "str_match" :: _ | "str_match_set" :: _ =>
    -- consume the nested lines; not modeled
    skipDeeper indent
    return .unsupported t
  | ["jump", j] => match natAfter "j" j with
    | some jid => return .jump jid
    | none => throw s!"line {l.no}: bad jump"
  | ["ret", x] => match parseLocal x with
    | some xl => return .ret xl
    | none => throw s!"line {l.no}: bad ret"
  | ["crash", x] => return .crash (parseLocal x)
  | "crash" :: _ => return .crash none
  | ["runtime_error"] => return .runtimeError
  | ["expect_err", x] => return .crash (parseLocal x)
  | "expect_err" :: _ => return .crash none
  | "comptime_exhaustiveness_failed" :: _ => return .crash none
  | ["<<already", "printed>>"] => return .unsupported t
  | _ =>
    let k ← simple l
    match k with
    | .inl mk => return mk (← chain indent)
    | .inr terminal => return terminal
partial def skipDeeper (indent : Nat) : P Unit := do
  repeat
    match (← peek) with
    | some c => if c.indent > indent then advance else break
    | none => break

/-- A non-control statement: returns a constructor awaiting its successor. -/
partial def simple (l : Line) : P ((Stmt → Stmt) ⊕ Stmt) := do
  let t := l.text
  let ws := words t
  match ws with
  | "incref" :: x :: n :: _ => match parseLocal x, natAfter "x" n with
    | some xl, some c => return .inl (.inc xl c)
    | _, _ => throw s!"line {l.no}: bad incref"
  | "decref" :: x :: _ => match parseLocal x with
    | some xl => return .inl (.dec xl)
    | none => throw s!"line {l.no}: bad decref"
  | "free" :: x :: _ => match parseLocal x with
    | some xl => return .inl (.free xl)
    | none => throw s!"line {l.no}: bad free"
  | "decref_if_initialized" :: _ =>
    match (field "cond" ws).bind parseLocal, (field "value" ws).bind parseLocal with
    | some c, some v => return .inl (.decIfInit c (parseHex ((field "mask" ws).getD "0")) v)
    | _, _ => throw s!"line {l.no}: bad decref_if_initialized"
  | ["init_uninitialized", x] => match parseLocal x with
    | some xl => return .inl (.initUninit xl)
    | none => throw s!"line {l.no}: bad init_uninitialized"
  | "set" :: a :: ":=" :: b :: _ => match parseLocal a, parseLocal b with
    | some x, some y => return .inl (.set x y)
    | _, _ => throw s!"line {l.no}: bad set"
  | ["debug", x] | ["expect", x] => match parseLocal x with
    | some xl => return .inl (.use xl)
    | none => throw s!"line {l.no}: bad debug/expect"
  | _ =>
    match t.splitOn " = " with
    | lhs :: rhsParts =>
      match parseTypedLocal lhs with
      | some (x, ty) => return .inl (.assign x ty (parseRhs (" = ".intercalate rhsParts)))
      | none => return .inr (.unsupported t)
    | [] => return .inr (.unsupported t)
end

/-- `=== LIR name (p3, identity=…, fingerprint=…, borrowed=1, ret_borrowed=false, unique=false) ===` -/
def parseHeader (t : String) : Option (String × ProcId × Nat × Bool × Bool) := do
  let body := (t.drop 8).toString                 -- after "=== LIR "
  let open_ := (body.splitOn " (p")
  let name := open_.head!
  let rest := " (p".intercalate open_.tail
  let ws := (rest.replace "," " ").replace ")" " " |> words
  let p ← (ws.head?).bind (·.toNat?)
  let borrowed := parseHex ((field "borrowed" ws).getD "0")
  let rb := (field "ret_borrowed" ws) == some "true"
  let un := (field "unique" ws) == some "true"
  pure (name, p, borrowed, rb, un)

def rcLayoutsOfLine (s : String) : List Nat :=
  ((s.splitOn "rc=concrete(").drop 1).filterMap fun piece =>
    match piece.splitOn "," with
    | _ :: rest :: _ => ((rest.splitOn ")").head!).toNat?
    | _ => none

def parseProcs : P (List Proc) := do
  let mut out : Array Proc := #[]
  repeat
    match (← peek) with
    | none => break
    | some l =>
      if l.indent == 0 && l.text.startsWith "=== LIR " then
        advance
        let some (name, pid, borrowed, rb, un) := parseHeader l.text
          | throw s!"line {l.no}: bad header"
        let some hl ← peek | throw "missing proc line"
        advance
        let some (ps, after) := bracketed hl.text '[' ']' | throw s!"line {hl.no}: bad proc line"
        let params := (splitTop ps).filterMap parseTypedLocal
        let retTy := match (words after).find? (·.startsWith "ret=") with
          | some r => parseTy (r.drop 4).toString
          | none => parseTy "zst"
        let startPos := (← get).pos
        let nxt ← peek
        let body ← match nxt with
          | some b =>
            if b.indent == 1 && b.text == "<no body>" then do
              advance
              pure (Stmt.unsupported "no body")
            else chain 1
          | none => pure (Stmt.unsupported "no body")
        let endPos := (← get).pos
        let lines := ((← get).lines.extract startPos endPos).toList
        let helpers := lines.flatMap fun ln => rcLayoutsOfLine ln.text
        out := out.push { id := pid, name, params, retTy, borrowedMask := borrowed,
                          retBorrowed := rb, retUnique := un, body, rcHelperLayouts := helpers }
      else advance   -- compiler output that is not part of a dump
  return out.toList

/-- Lines of the dump. Lines outside `=== LIR` blocks are skipped by `parseProcs`
    only at indent 0; anything indented that is not part of a proc is an error. -/
def toLines (s : String) : Array Line := Id.run do
  let mut out := #[]
  let mut no := 1
  let mut inProc := false
  for raw in s.splitOn "\n" do
    let r := trimRight raw
    if r.startsWith "=== LIR " then inProc := true
    else if r == "" then inProc := false
    if r != "" && (inProc || r.startsWith "=== LIR ") then
      out := out.push { indent := indentOf r, text := (trim r), no }
    no := no + 1
  return out

end Parse

def parseDump (s : String) : Except String Program := do
  let (procs, _) ← (Parse.parseProcs).run { lines := Parse.toLines s, pos := 0 }
  return { procs }

/-- Layout ids named by any RC helper in the dump: `rc=concrete(op,ID)`. -/
def rcLayoutsOf (s : String) : List Nat :=
  ((s.splitOn "rc=concrete(").drop 1).filterMap fun piece =>
    match piece.splitOn "," with
    | _ :: rest :: _ => ((rest.splitOn ")").head!).toNat?
    | _ => none

end RocArc

namespace RocArc
/-- One `ROC_LIR_DUMP` output holds every LIR program the compiler lowered
    (compile-time evaluation, each test batch, …). Each program prints its procs
    in increasing id order, so a non-increasing id starts the next program. -/
def splitPrograms (P : Program) : List Program := Id.run do
  let mut out : Array Program := #[]
  let mut cur : Array Proc := #[]
  let mut last : Option Nat := none
  for p in P.procs do
    match last with
    | some l => if p.id ≤ l then
        out := out.push { procs := cur.toList }
        cur := #[]
      | none => pure ()
    cur := cur.push p
    last := some p.id
  if cur.size > 0 then out := out.push { procs := cur.toList }
  return out.toList
end RocArc
