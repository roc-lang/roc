app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/10219: reading a module-level
# list constant must reference its one static materialization without allocating.
bases : List(U64)
bases = [1, 2, 3]

run! : Str => Str
run! = |input| {
	input_bytes = Str.to_utf8(input)
	index = input_bytes.len() % 3

	before = Host.alloc_count!()
	value = bases.get(index) ?? 0
	read_allocs = Host.alloc_count!() - before

	expect value == 2
	expect read_allocs == 0

	"value: ${value.to_str()}, read allocations: ${read_allocs.to_str()}"
}
