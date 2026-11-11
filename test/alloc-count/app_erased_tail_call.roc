app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A call through a boxed function in tail position is left pending for its
# caller to make. Recording that pending call must not heap-allocate a copy of
# small argument blocks. A block too large for the pending record is copied to
# the heap, and a chain of such calls reuses its blocks: it holds two however
# long it runs, the one the running call reads and the one it leaves pending.
# (The interpreter keeps pending calls in its own reused storage, which the
# host does not count.)
apply_one : Box((U64 -> U64)), U64 -> U64
apply_one = |boxed, x| {
	f = Box.unbox(boxed)
	f(x)
}

apply_str : Box((Str, U64 -> U64)), Str, U64 -> U64
apply_str = |boxed, s, x| {
	f = Box.unbox(boxed)
	f(s, x)
}

Wide : { a : U64, b : U64, c : U64, d : U64, e : U64, f : U64, g : U64, h : U64, i : U64, j : U64, k : U64, l : U64, m : U64, n : U64, o : U64, p : U64, q : U64, r : U64, s : U64, t : U64 }

apply_wide : Box((Wide -> U64)), Wide -> U64
apply_wide = |boxed, w| {
	f = Box.unbox(boxed)
	f(w)
}

# Each step leaves the next one pending through a boxed function, so one
# waiter makes the whole chain of calls.
count_down_wide : Wide -> U64
count_down_wide = |w|
	if w.a == 0 {
		w.t
	} else {
		apply_wide(Box.box(count_down_wide), { ..w, a: w.a - 1, t: w.t + w.b })
	}

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input)

	one : Box((U64 -> U64))
	one = Box.box(|x| x + 1)
	before_one = Host.alloc_count!()
	var $a = n
	var $i = 0
	while $i < 1000 {
		$a = apply_one(one, $a)
		$i = $i + 1
	}
	one_allocs = Host.alloc_count!() - before_one

	str : Box((Str, U64 -> U64))
	str = Box.box(|s, x| x + Str.count_utf8_bytes(s))
	label = "ab"
	before_str = Host.alloc_count!()
	var $b = n
	var $j = 0
	while $j < 1000 {
		$b = apply_str(str, label, $b)
		$j = $j + 1
	}
	str_allocs = Host.alloc_count!() - before_str

	wide : Box((Wide -> U64))
	wide = Box.box(|w| w.a + w.t)
	record = { a: 9, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10, k: 11, l: 12, m: 13, n: 14, o: 15, p: 16, q: 17, r: 18, s: 19, t: 1 }
	var $c = 0
	var $k = 0
	while $k < 1000 {
		$c = $c + apply_wide(wide, record)
		$k = $k + 1
	}

	before_chain = Host.alloc_count!()
	chain = count_down_wide({ ..record, a: 1000 + n, t: 0 })
	chain_allocs = Host.alloc_count!() - before_chain

	expect $a == n + 1000
	expect $b == n + 2000
	"results: ${($a - n).to_str()} ${($b - n).to_str()} ${$c.to_str()}, allocations: one argument ${one_allocs.to_str()}, string argument ${str_allocs.to_str()}, wide chain ${(chain - 2 * n).to_str()} with ${chain_allocs.to_str()}"
}
