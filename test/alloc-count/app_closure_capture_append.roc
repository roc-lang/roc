app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/13012: an erased call moves a dying
# closure into its body, which takes the captured list out of it and appends
# to it in place. The closure type is boxed, so every call goes through the
# erased calling convention, and the list is reserved up front, so the loop
# allocates exactly as much as the same loop over a closure capturing a
# scalar.
make_list : List(U64) -> (U64 -> List(U64))
make_list = |xs| |i| xs.append(i)

make_scalar : U64 -> (U64 -> U64)
make_scalar = |n| |i| n + i

run! : Str => Str
run! = |input| {
	extra = Str.count_utf8_bytes(input)
	start = List.with_capacity(2000 + extra)

	before_list = Host.alloc_count!()
	var $list = make_list(start)
	for i in 0..<1000 {
		$list = make_list($list(i))
	}
	list_allocs = Host.alloc_count!() - before_list

	before_scalar = Host.alloc_count!()
	var $scalar = make_scalar(extra)
	for i in 0..<1000 {
		$scalar = make_scalar($scalar(i))
	}
	scalar_allocs = Host.alloc_count!() - before_scalar

	final = $list(1000)
	_kept = Box.box($list)
	_kept_scalar = Box.box($scalar)
	expect List.len(final) == 1001
	expect List.get(final, 1000) == Ok(1000)

	same = if list_allocs == scalar_allocs "same" else "different"
	"1000 erased calls appending to a captured list: ${same} allocations as a scalar closure (${list_allocs.to_str()}, ${scalar_allocs.to_str()})"
}
