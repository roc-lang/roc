app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/13012. A closure that
# captures a list, called in a loop that builds the next closure from the
# call's result, owns the only reference to its list, so `List.set` updates
# it in place. That holds whether the closure is kept in a box (unboxed,
# called, and its successor boxed again) or called directly in a program that
# boxes a value of its type elsewhere. Each loop therefore allocates exactly
# as much as the same loop over a closure that captures a scalar.
make_list : List(U64) -> (U64 -> List(U64))
make_list = |xs| |i| xs.set(i % xs.len(), i) ?? xs

make_scalar : U64 -> (U64 -> U64)
make_scalar = |n| |i| n + i

compare : U64, U64 -> Str
compare = |list, scalar| {
	verdict = if list == scalar "same" else "different"
	"${verdict} allocations (list closure ${list.to_str()}, scalar closure ${scalar.to_str()})"
}

run! : Str => Str
run! = |input| {
	size = 10_000 + Str.count_utf8_bytes(input)

	boxed_start = List.repeat(0, size)
	before_boxed_list = Host.alloc_count!()
	var $hl = Box.box(make_list(boxed_start))
	for i in 0..<1000 {
		f = Box.unbox($hl)
		$hl = Box.box(make_list(f(i)))
	}
	boxed_list = Host.alloc_count!() - before_boxed_list

	before_boxed_scalar = Host.alloc_count!()
	var $hs = Box.box(make_scalar(size))
	for i in 0..<1000 {
		f = Box.unbox($hs)
		$hs = Box.box(make_scalar(f(i)))
	}
	boxed_scalar = Host.alloc_count!() - before_boxed_scalar

	plain_start = List.repeat(0, size)
	before_plain_list = Host.alloc_count!()
	var $fl = make_list(plain_start)
	for i in 0..<1000 {
		$fl = make_list($fl(i))
	}
	plain_list = Host.alloc_count!() - before_plain_list

	before_plain_scalar = Host.alloc_count!()
	var $fs = make_scalar(size)
	for i in 0..<1000 {
		$fs = make_scalar($fs(i))
	}
	plain_scalar = Host.alloc_count!() - before_plain_scalar

	"1000 calls through a box: ${compare(boxed_list, boxed_scalar)}; 1000 direct calls: ${compare(plain_list, plain_scalar)}"
}
