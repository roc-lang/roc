app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A derived JSON encoder writes a `Set` as the list of its items. The list is
# built once for the whole encode, so the encode's allocation count grows with
# the log of its output, like a list's encode, not with the item count: 16x
# as many items cost only a handful more allocations.
run! : Str => Str
run! = |input| {
	# Both sizes derive from the hosted input, so neither encode folds away.
	seed = Str.count_utf8_bytes(input)
	small = build_set(seed * 4)
	large = build_set(seed * 64)

	small_before = Host.alloc_count!()
	small_json = Json.to_str({ items: small })
	small_allocs = Host.alloc_count!() - small_before

	large_before = Host.alloc_count!()
	large_json = Json.to_str({ items: large })
	large_allocs = Host.alloc_count!() - large_before

	expect Str.starts_with(small_json, "{\"items\":[")
	expect Str.count_utf8_bytes(large_json) > Str.count_utf8_bytes(small_json)
	expect large_allocs <= small_allocs + 8
	"set encode: ${(large_allocs - small_allocs).to_str()} more allocations for 16x the items"
}

build_set : U64 -> Set(U64)
build_set = |count| {
	var $set = Set.with_capacity(count)
	var $i = 0
	while $i < count {
		$set = $set.insert($i)
		$i = $i + 1
	}
	$set
}
