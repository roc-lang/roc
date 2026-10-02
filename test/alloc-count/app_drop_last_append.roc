app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for issue 11965. Each step pushes x, drops it, and pushes it again on
# a uniquely owned list, so appends after `drop_last` reuse the allocation and
# the loop allocates only when capacity grows. `step_b` gives `List.drop_last`
# a second call site on the same element type.
step_a : List(U64), U64 -> List(U64)
step_a = |list, x| list.append(x).drop_last(1).append(x)

step_b : List(U64), U64 -> List(U64)
step_b = |list, x| list.append(x).drop_last(1).append(x)

run_steps : (List(U64), U64 -> List(U64)), U64 -> List(U64)
run_steps = |step, n| {
	var $list = List.with_capacity(n + 1)
	var $i = 0
	while $i < n {
		$list = step($list, $i)
		$i = $i + 1
	}
	$list
}

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input) * 64

	a_before = Host.alloc_count!()
	a = run_steps(step_a, n)
	a_allocs = Host.alloc_count!() - a_before

	b_before = Host.alloc_count!()
	b = run_steps(step_b, n)
	b_allocs = Host.alloc_count!() - b_before

	"drop_last steps: ${List.len(a).to_str()} ${List.len(b).to_str()}, allocations: ${a_allocs.to_str()} ${b_allocs.to_str()}"
}
