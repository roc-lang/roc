app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Variants of https://github.com/roc-lang/roc/issues/12957. A closure captures
# a record whose lists sit further down a path of records or a tag payload,
# and each call writes one list and returns a new closure built around the
# result. The closure lives in a list: each step takes it out with
# `List.replace`, calls it, and puts the returned closure back. Nothing else
# refers to the lists, so every `List.set` must update in place. Each call
# also returns the element it wrote, so the sums check the values.
Step := [Step((U64 -> (Step, U64)))]

Cols : { items : List(U64), other : List(U64) }

Wrap : [Wrap(Cols), Empty]

get0 : List(U64), U64 -> U64
get0 = |l, i| l.get(i) ?? 0

# Three levels: captured record, then `a`, then `b`, then the lists.
deep3 : Cols -> Step
deep3 = |cols0| {
	held = { a: { b: cols0 } }
	Step.Step(
		|i| {
			{ items, other } = held.a.b
			next = items.set(i, i + 1) ?? crash "items"
			(deep3({ items: next, other }), get0(next, i))
		},
	)
}

# Four levels with refcounted siblings at every level, each read again after
# the write.
deep4 : Cols, List(U64), Str -> Step
deep4 = |cols0, side, s| {
	held = { a: { b: { c: cols0, side2: side }, side1: s }, n: 7 }
	Step.Step(
		|i| {
			{ items, other } = held.a.b.c
			next = items.set(i, i + held.n) ?? crash "items"
			(deep4({ items: next, other }, held.a.b.side2, held.a.side1), get0(next, i) + held.a.b.side2.len() + held.a.side1.count_utf8_bytes())
		},
	)
}

# A tag union payload on the path.
tagged : Wrap -> Step
tagged = |w0| {
	held = { w: w0, k: 3 }
	Step.Step(
		|i| {
			match held.w {
				Wrap({ items, other }) => {
					next = items.set(i, i + held.k) ?? crash "items"
					(tagged(Wrap({ items: next, other })), get0(next, i))
				}
				Empty => (tagged(Empty), 0)
			}
		},
	)
}

# Each branch reads the path again and writes a different list.
branchy : Cols -> Step
branchy = |cols0| {
	held = { cols: cols0, n: 1 }
	Step.Step(
		|i| {
			if i % 2 == 0 {
				{ items, other } = held.cols
				next = items.set(i, i + held.n) ?? crash "items"
				(branchy({ items: next, other }), get0(next, i))
			} else {
				{ items, other } = held.cols
				next = other.set(i, i + held.n) ?? crash "other"
				(branchy({ items, other: next }), get0(next, i))
			}
		},
	)
}

placeholder : {} -> Step
placeholder = |_| Step.Step(|_| (placeholder({}), 0))

measure! : Step => (U64, U64)
measure! = |start| {
	var $kids = [start]
	var $sum = 0
	before = Host.alloc_count!()
	var $i = 0
	while $i < 100 {
		{ list, prev } = $kids.replace(0, placeholder({})) ?? crash "kid"
		Step.Step(call) = prev
		(next, v) = call($i)
		$sum = $sum + v
		$kids = list.set(0, next) ?? crash "kid"
		$i = $i + 1
	}
	(Host.alloc_count!() - before, $sum)
}

show : (U64, U64) -> Str
show = |(allocs, sum)| "${allocs.to_str()} allocations, sum ${sum.to_str()}"

run! : Str => Str
run! = |input| {
	n = 1_000 + input.count_utf8_bytes()
	cols = |_| { items: List.repeat(0, n), other: List.repeat(0, n) }
	"100 calls: deep3 ${show(measure!(deep3(cols({}))))}; deep4 ${show(measure!(deep4(cols({}), [1, 2, 3], "hello")))}; tagged ${show(measure!(tagged(Wrap(cols({})))))}; branchy ${show(measure!(branchy(cols({}))))}"
}
