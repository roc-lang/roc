app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12846. A closure captures a
# record holding a list. Each call passes the record to `step`, which matches
# on `List.replace`, rebuilds the record, and returns it in a tuple; the closure
# is then rebuilt from the result. Nothing else references the record or the
# list, so every call must update the list in place.
Block : { tag : U64, rows : List(U64) }

Root := [Root((U64 -> Root))]

step : Block, U64 -> (Block, U64)
step = |{ tag, rows }, x| {
	match rows.replace(0, 0) {
		Ok({ list, prev }) => ({ tag, rows: list.set(0, prev + x) ?? crash "rows" }, x)
		Err(_) => crash "rows"
	}
}

root : Block -> Root
root = |block| Root(|x| {
	(next, _) = step(block, x)
	root(next)
})

run! : Str => Str
run! = |input| {
	n = 10_000 + input.count_utf8_bytes()
	var $r = root({ tag: 0, rows: List.repeat(0, n) })
	before = Host.alloc_count!()
	var $i = 0
	while $i < 1000 {
		Root(f) = $r
		$r = f($i)
		$i = $i + 1
	}
	"1000 calls: ${(Host.alloc_count!() - before).to_str()} allocations"
}
