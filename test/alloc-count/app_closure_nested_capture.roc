app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12957. A closure captures a
# record (`held`) whose one field is a record of two lists. Each call writes
# one element of one list and returns a new closure built around the result.
# The closure lives in a list: each step takes it out with `List.replace`,
# calls it, and puts the returned closure back. Nothing else refers to the
# lists, so every `List.set` must update in place.
Step := [Step((U64 -> Step))]

Cols : { items : List(U64), other : List(U64) }

nested : Cols -> Step
nested = |cols0| {
	held = { cols: cols0 }
	Step.Step(
		|i| {
			{ items, other } = held.cols
			nested({ items: items.set(i, i) ?? crash "items", other })
		},
	)
}

# The same, with the two lists as fields of `held` itself.
flat : Cols -> Step
flat = |cols0| {
	held = { items: cols0.items, other: cols0.other }
	Step.Step(
		|i| {
			{ items, other } = held
			flat({ items: items.set(i, i) ?? crash "items", other })
		},
	)
}

placeholder : {} -> Step
placeholder = |_| Step.Step(|_| placeholder({}))

allocations! : Step => U64
allocations! = |start| {
	var $kids = [start]
	before = Host.alloc_count!()
	var $i = 0
	while $i < 100 {
		{ list, prev } = $kids.replace(0, placeholder({})) ?? crash "kid"
		Step.Step(call) = prev
		$kids = list.set(0, call($i)) ?? crash "kid"
		$i = $i + 1
	}
	Host.alloc_count!() - before
}

run! : Str => Str
run! = |input| {
	n = 10_000 + input.count_utf8_bytes()
	cols = |_| { items: List.repeat(0, n), other: List.repeat(0, n) }
	"100 calls: nested ${allocations!(nested(cols({}))).to_str()} allocations, flat ${allocations!(flat(cols({}))).to_str()} allocations"
}
