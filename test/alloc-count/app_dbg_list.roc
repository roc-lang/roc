app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# repro for https://github.com/roc-lang/roc/issues/10967
#
# Structural inspection appends every rendered element into one string. That
# accumulator must grow geometrically, so inspecting 16x as many elements costs
# only a handful more allocations rather than one allocation per appended part.
run! : Str => Str
run! = |input| {
	seed = Str.count_utf8_bytes(input)

	small : List(U64)
	small = List.repeat(16, seed * 4)

	large : List(U64)
	large = List.repeat(16, seed * 64)

	small_before = Host.alloc_count!()
	dbg small
	small_allocs = Host.alloc_count!() - small_before

	large_before = Host.alloc_count!()
	dbg large
	large_allocs = Host.alloc_count!() - large_before

	small_len = List.len(small)
	large_len = List.len(large)

	expect small_len == seed * 4
	expect large_len == seed * 64
	expect large_allocs <= small_allocs + 32

	"dbg elements: ${small_len.to_str()} ${large_len.to_str()}, allocations: ${small_allocs.to_str()} ${large_allocs.to_str()}"
}
