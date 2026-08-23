app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# repro for https://github.com/roc-lang/roc/issues/10484
#
# `Json.to_str` appends every element into one accumulator, and that accumulator
# grows geometrically, so an encode's allocation count scales with the log of
# the bytes it produces. Encoding 16x as many elements therefore costs only a
# handful more allocations than the small encode, not 16x as many.
#
# When each element's append instead sees the accumulator as shared, it copies
# the whole buffer into a fresh exact-sized allocation, which makes allocations
# scale with the element count and bytes copied scale with its square.
run! : Str => Str
run! = |input| {
	# Both element counts derive from the hosted input, so no part of either
	# encode can be folded away at compile time.
	seed = Str.count_utf8_bytes(input)

	# Built by an append loop rather than `List.repeat`: appending is separately
	# covered as allocation-amortized, and both lists are built before any
	# measurement starts, so neither build lands in a measured region.
	small_items = build_items(seed * 4)
	large_items = build_items(seed * 64)

	small_before = Host.alloc_count!()
	small_json = Json.to_str(small_items)
	small_allocs = Host.alloc_count!() - small_before

	large_before = Host.alloc_count!()
	large_json = Json.to_str(large_items)
	large_allocs = Host.alloc_count!() - large_before

	small_bytes = Str.count_utf8_bytes(small_json)
	large_bytes = Str.count_utf8_bytes(large_json)

	# Each element encodes to `"abcdefghij"` plus a separating comma, and the
	# brackets replace the comma the last element does not need.
	expect small_bytes == seed * 4 * 13 + 1
	expect large_bytes == seed * 64 * 13 + 1

	# The geometric steps between the two output sizes are what the larger
	# encode may spend beyond the smaller one. The bound is loose enough to
	# absorb the fixed per-encode allocations at both sizes and still far below
	# the one-allocation-per-element cost of recopying the accumulator.
	expect large_allocs <= small_allocs + 32

	"json bytes: ${small_bytes.to_str()} ${large_bytes.to_str()}, allocations: ${small_allocs.to_str()} ${large_allocs.to_str()}"
}

build_items : U64 -> List(Str)
build_items = |n| {
	var $items = []
	var $i = 0
	while $i < n {
		$items = List.append($items, "abcdefghij")
		$i = $i + 1
	}
	$items
}
