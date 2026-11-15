app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Ownership of a closure's captures through erased calls
# (https://github.com/roc-lang/roc/issues/13012). Every closure type here is
# boxed somewhere, so its calls go through the erased calling convention.
#
# A closure that stays alive across its calls lends its captures to each call:
# a captured list updated by one call is unchanged for the next call and for
# every other holder of the closure. A closure that dies at its call hands its
# captures to the body: the list updated there is updated in place, while the
# body still reads every other capture, before and after the update.
make_list : List(U64) -> (U64 -> List(U64))
make_list = |xs| |i| xs.set(i % xs.len(), i) ?? xs

make_pair : List(U64), List(U64), Str -> (U64 -> List(U64))
make_pair = |xs, ys, label| |i| {
	before = ys.len() + Str.count_utf8_bytes(label)
	updated = xs.set(i % xs.len(), i + before) ?? xs
	after = ys.len() + before
	updated.set(0, after) ?? updated
}

make_scalar : U64 -> (U64 -> U64)
make_scalar = |n| |i| n + i

sum : List(U64) -> U64
sum = |items| items.fold(0, |acc, item| acc + item)

run! : Str => Str
run! = |input| {
	size = 10 + Str.count_utf8_bytes(input)

	# One closure called many times, kept alive by a box and by a local.
	shared = make_list(List.repeat(0, size))
	boxed = Box.box(shared)
	var $shared_total = 0
	for i in 0..<1000 {
		f = Box.unbox(boxed)
		$shared_total = $shared_total + sum(f(i)) + sum(shared(i + 1))
	}
	untouched = Box.unbox(boxed)(size)
	expect sum(untouched) == size
	expect sum(shared(size + 1)) == size + 1
	expect $shared_total == 1000000

	# A dying closure with several captures: one updated, the others read.
	ys = List.repeat(5, 7)
	start = List.repeat(0, size)
	before_pair = Host.alloc_count!()
	var $pair = make_pair(start, ys, "abc")
	for i in 0..<1000 {
		$pair = make_pair($pair(i), ys, "abc")
	}
	pair_allocs = Host.alloc_count!() - before_pair
	result = $pair(1000)
	expect List.len(result) == size
	expect List.get(result, 0) == Ok(17)
	expect sum(ys) == 35

	before_scalar = Host.alloc_count!()
	var $scalar = make_scalar(size)
	for i in 0..<1000 {
		$scalar = make_scalar($scalar(i))
	}
	scalar_allocs = Host.alloc_count!() - before_scalar
	_kept = Box.box($pair)
	_kept_scalar = Box.box($scalar)

	same = if pair_allocs == scalar_allocs "same" else "different"
	"shared closure total: ${$shared_total.to_str()}; dying closure with several captures: ${same} allocations as a scalar closure (${pair_allocs.to_str()}, ${scalar_allocs.to_str()})"
}
