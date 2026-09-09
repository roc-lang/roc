app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

measure_allocs! : ({} => a) => { value : a, allocations : U64 }
measure_allocs! = |body!| {
	before = Host.alloc_count!()
	value = body!({})
	after = Host.alloc_count!()
	{ value, allocations: after - before }
}

# A uniquely owned, pre-sized Dict must remain unique when ownership moves into
# a closure passed to a higher-order function. The closure boundary must not
# introduce a second live owner that makes every insert copy the backing lists.
run! : Str => Str
run! = |input| {
	count = 1000 + Str.count_utf8_bytes(input)
	start = Dict.with_capacity(count)

	measured = measure_allocs!(
		|{}| {
			var $dict = start
			var $index = 0
			while $index < count {
				$dict = Dict.insert($dict, $index, $index * 2)
				$index = $index + 1
			}
			$dict
		},
	)
	expect measured.allocations == 0
	expect Dict.len(measured.value) == count
	expect Dict.get(measured.value, count - 1) == Ok((count - 1) * 2)

	"entries: ${Dict.len(measured.value).to_str()}, closure insert allocations: ${measured.allocations.to_str()}"
}
