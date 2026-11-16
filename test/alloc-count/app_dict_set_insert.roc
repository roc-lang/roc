app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/13002: a uniquely owned `Dict` or
# `Set` that a loop inserts new keys into is updated in place, so the loop
# allocates far less than once per insert: a pre-sized `Dict` allocates only
# where a `.boxy` probe's frame-replacing call carries its dynamic arguments,
# and a `Set` also allocates where its storage grows. A copying insert would
# allocate at least twice per insert, once for each of its two lists.
# Compile-time evaluation runs the same `.boxy` code as a `--specialize=no`
# build.
run! : Str => Str
run! = |input| {
	count = 2000 + Str.count_utf8_bytes(input)

	var $dict = Dict.with_capacity(count)
	before_dict = Host.alloc_count!()
	var $j = 0
	while $j < count {
		$dict = Dict.insert($dict, $j, $j)
		$j = $j + 1
	}
	dict_allocs = Host.alloc_count!() - before_dict

	before_set = Host.alloc_count!()
	var $set = Set.empty()
	var $i = 0
	while $i < count {
		$set = Set.insert($set, $i)
		$i = $i + 1
	}
	set_allocs = Host.alloc_count!() - before_set

	expect Dict.len($dict) == count
	expect Dict.get($dict, count - 1) == Ok(count - 1)
	expect Set.len($set) == count
	expect Set.contains($set, count - 1)

	dict_verdict = if dict_allocs * 2 < count "fewer than half" else "at least half"
	set_verdict = if set_allocs * 2 < count "fewer than half" else "at least half"
	"${count.to_str()} inserts: Dict allocations ${dict_verdict} the inserts, Set allocations ${set_verdict} the inserts"
}
