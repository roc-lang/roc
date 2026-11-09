app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# repro for https://github.com/roc-lang/roc/issues/12328
# `pick` returns a record whose `cur` field holds a list. Reading `id.cur`
# takes that field out of the dying result, and passing it on to `advance`
# hands the taken list to the owned variant of `advance`, which moves the
# list out of its argument in turn. The list must be released exactly once.

Cur : { bytes : List(U8), pos : U64 }

pick : Cur -> { n : U64, cur : Cur }
pick = |cur| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte), cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

advance : Cur -> { n : U64, cur : Cur }
advance = |cur| pick(cur)

once : List(U8) -> U64
once = |bytes| {
	id = pick({ bytes, pos: 0 })
	next = advance(id.cur)
	id.n + next.n + next.cur.pos
}

looped : List(U8) -> U64
looped = |bytes| {
	var $cur = { bytes, pos: 0 }
	var $sum = 0
	var $index = 0
	while $index < 3 {
		id = pick($cur)
		next = advance(id.cur)
		$cur = next.cur
		$sum = $sum + id.n + next.n
		$index = $index + 1
	}
	$sum + $cur.pos
}

main! = || {
	bytes = Stdin.line!().to_utf8()
	Stdout.line!("once: ${once(bytes).to_str()}")
	Stdout.line!("looped: ${looped(bytes).to_str()}")
}
