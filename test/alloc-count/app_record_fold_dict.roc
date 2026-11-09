app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

measure_allocs! : ({} => a) => { value : a, allocations : U64 }
measure_allocs! = |body!| {
	before = Host.alloc_count!()
	value = body!({})
	after = Host.alloc_count!()
	{ value, allocations: after - before }
}

# A fold whose state record holds a List next to a Dict must insert into the
# Dict in place. The state is uniquely owned on every step, so each insert
# into the pre-sized Dict must see a unique Dict and allocate nothing.
run! : Str => Str
run! = |input| {
	count = 1000 + Str.count_utf8_bytes(input)
	keys = List.repeat(0, count).map_with_index(|_, i| i)
	start = { list: [1, 2, 3], dict: Dict.with_capacity(count) }

	measured = measure_allocs!(
		|{}| keys.fold(start, |acc, k| { list: acc.list, dict: acc.dict.insert(k, k * 2) }),
	)
	expect measured.allocations == 0
	expect Dict.len(measured.value.dict) == count
	expect Dict.get(measured.value.dict, count - 1) == Ok((count - 1) * 2)
	expect measured.value.list == [1, 2, 3]

	"entries: ${Dict.len(measured.value.dict).to_str()}, record fold insert allocations: ${measured.allocations.to_str()}"
}
