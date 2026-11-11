app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12836
# Boxing a callable that has no captures must not heap-allocate: the boxed
# value is immutable and capture-free, so it lives in static storage with the
# static refcount, and dropping or sharing it never frees anything. A boxed
# capturing closure still allocates once per box.
inc : U64 -> U64
inc = |x| x + 1

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input)

	before_lambda = Host.alloc_count!()
	var $a = n
	var $i = 0
	while $i < 1000 {
		boxed : Box((U64 -> U64))
		boxed = Box.box(|x| x + 1)
		f = Box.unbox(boxed)
		$a = f($a)
		$i = $i + 1
	}
	lambda_allocs = Host.alloc_count!() - before_lambda

	before_top = Host.alloc_count!()
	var $b = n
	var $j = 0
	while $j < 1000 {
		boxed : Box((U64 -> U64))
		boxed = Box.box(inc)
		f = Box.unbox(boxed)
		g = Box.unbox(boxed)
		$b = g(f($b))
		$j = $j + 1
	}
	top_allocs = Host.alloc_count!() - before_top

	before_capture = Host.alloc_count!()
	var $c = n
	var $k = 0
	while $k < 1000 {
		step = $k
		boxed : Box((U64 -> U64))
		boxed = Box.box(|x| x + step)
		f = Box.unbox(boxed)
		$c = f($c)
		$k = $k + 1
	}
	capture_allocs = Host.alloc_count!() - before_capture

	expect $a == n + 1000
	expect $b == n + 2000
	expect $c == n + 499500
	"results: ${($a - n).to_str()} ${($b - n).to_str()} ${($c - n).to_str()}, allocations: capture-free lambda ${lambda_allocs.to_str()}, top-level function ${top_allocs.to_str()}, capturing closure ${capture_allocs.to_str()}"
}
