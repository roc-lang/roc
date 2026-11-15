app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# An erased call whose result holds exactly one erased-callable slot receives
# the dying closure's allocation and repacks it as the closure in its result,
# whether the result is the closure itself or a record holding it. Each
# program allocates its boxes and the first closure, and nothing for the
# closure the call returns.
make_same : Box(I64) -> Box(({} -> ({} -> I64)))
make_same = |state| Box.box(|_| |_| Box.unbox(state))

make_aggregate : Box(I64) -> Box(({} -> { next : ({} -> I64), observed : I64 }))
make_aggregate = |state| Box.box(|_| {
	next_state = Box.box(Box.unbox(state) + 1)
	{ next: |_| Box.unbox(next_state), observed: Box.unbox(state) }
})

run! : Str => Str
run! = |input| {
	seed = 41 + Str.count_utf8_bytes(input).to_i64_wrap()

	before_same = Host.alloc_count!()
	same_value = Box.unbox(make_same(Box.box(seed + 1)))({})({})
	same_allocs = Host.alloc_count!() - before_same

	before_chain = Host.alloc_count!()
	a00 = make_same(Box.box(seed + 1))
	a01 = a00
	a02 = a01
	a03 = a02
	a04 = a03
	a05 = a04
	a06 = a05
	a07 = a06
	a08 = a07
	a09 = a08
	chain_value = Box.unbox(a09)({})({})
	chain_allocs = Host.alloc_count!() - before_chain

	before_aggregate = Host.alloc_count!()
	result = Box.unbox(make_aggregate(Box.box(seed)))({})
	aggregate_ok = result.observed == seed and (result.next)({}) == seed + 1
	aggregate_allocs = Host.alloc_count!() - before_aggregate

	expect same_value == seed + 1
	expect chain_value == seed + 1
	expect aggregate_ok

	"same-shape result: ${same_allocs.to_str()} allocations; owner chain: ${chain_allocs.to_str()} allocations; aggregate result: ${aggregate_allocs.to_str()} allocations"
}
