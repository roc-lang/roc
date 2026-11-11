app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12836
# A top-level constant holding a boxed capture-free callable is static data:
# reading it, unboxing it, and calling it never heap-allocates.
boxed_inc : Box((U64 -> U64))
boxed_inc = Box.box(|x| x + 1)

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input)

	before = Host.alloc_count!()
	var $a = n
	var $i = 0
	while $i < 1000 {
		f = Box.unbox(boxed_inc)
		$a = f($a)
		$i = $i + 1
	}
	allocs = Host.alloc_count!() - before

	expect $a == n + 1000
	"result: ${($a - n).to_str()}, constant allocations: ${allocs.to_str()}"
}
