app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

measure_allocs! : ({} => a) => { value : a, allocations : U64 }
measure_allocs! = |body!| {
	before = Host.alloc_count!()
	value = body!({})
	after = Host.alloc_count!()
	{ value, allocations: after - before }
}

filled_dict : U64 -> Dict(U64, U64)
filled_dict = |count| {
	var $dict = Dict.with_capacity(count)
	var $index = 0
	while $index < count {
		$dict = Dict.insert($dict, $index, $index)
		$index = $index + 1
	}
	$dict
}

run! : Str => Str
run! = |input| {
	count = 1000 + Str.count_utf8_bytes(input)
	start = filled_dict(count)
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

	"entries: ${Dict.len(measured.value).to_str()}, closure overwrite allocations: ${measured.allocations.to_str()}"
}
