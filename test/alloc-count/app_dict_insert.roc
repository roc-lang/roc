app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A uniquely owned Dict whose complete capacity is reserved up front should
# mutate its entries and Robin Hood bucket tables in place. The runtime input
# keeps the insertion count dynamic so compile-time evaluation cannot remove
# the work being measured.
run! : Str => Str
run! = |input| {
	count = 1000 + Str.count_utf8_bytes(input)
	var $dict = Dict.with_capacity(count)

	before = Host.alloc_count!()
	var $index = 0
	while $index < count {
		$dict = Dict.insert($dict, $index, $index * 2)
		$index = $index + 1
	}
	insert_allocs = Host.alloc_count!() - before

	expect insert_allocs == 0
	expect Dict.len($dict) == count
	expect Dict.get($dict, count - 1) == Ok((count - 1) * 2)

	"entries: ${Dict.len($dict).to_str()}, insert allocations: ${insert_allocs.to_str()}"
}
