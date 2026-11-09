# Repro for https://github.com/roc-lang/roc/issues/12438
# `generate` only reads `bytes`, so `main!` still owns the list afterward:
# reading it again is valid and it is released exactly once.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

Method : [Get, Post]

Kind : [Req({ method : Method, target : Str }), Resp({ status : U16 })]

Version : { major : U8, minor : U8 }

Field : { name : Str, value : Str }

Msg : { kind : Kind, start : List(U8), version : Version, fields : List(Field) }

Mutated : { bytes : List(U8), original : List(U8), kind : Kind, label : Str }

Result : Try({ bytes : List(U8), label : Str }, [NotApplicable])

Cur : { bytes : List(U8), pos : U64 }

field_named : Str, Str -> Field
field_named = |name, value| { name, value }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

gen : Cur, [Request, Response, Either] -> { msg : Msg, cur : Cur }
gen = |start, _want| {
	which = pick(start, 2)
	is_request = which.n == 0
	var $cur = which.cur
	if is_request {
		t = { text: "*", cur: $cur }
		$cur = t.cur
	}
	msg = { kind: Req({ method: Get, target: "*" }), start: [], version: { major: 1, minor: 0 }, fields: [] }
	{ msg, cur: $cur }
}

pick2 : Cur, U64 -> { n : U64, cur : Cur }
pick2 = |cur, count| pick(cur, count)

place : Cur, List(Field), Field -> List(Field)
place = |cur, fields, field| {
	at = pick(cur, fields.len() + 1)
	fields.append(field).drop_first(at.n)
}

pick_from : Cur, List(Str) -> Str
pick_from = |cur, options| options.get(pick2(cur, options.len()).n) ?? ""

mutate : U64, Msg, Cur -> Result
mutate = |_which, _msg, cur| {
	choice = pick(cur, 256)
	next = choice.cur
	fields = place(next, [], field_named("h", pick_from(next, ["other.example", "a", ""])))
	Ok({ bytes: [], label: if fields.is_empty() "" else "x" })
}

generate : List(U8) -> Try(Mutated, [NotApplicable])
generate = |bytes| {
	out = gen({ bytes, pos: 1 }, Either)
	_mutated = mutate(8, out.msg, out.cur)?
	Ok({ bytes: [], original: [], kind: Req({ method: Get, target: "" }), label: "" })
}

main! = || {
	bytes = Stdin.line!().to_utf8()
	ok = generate(bytes).is_ok()
	Stdout.line!("ok: ${Str.inspect(ok)}, len: ${bytes.len().to_str()}, first: ${(bytes.first() ?? 0).to_str()}")
}
