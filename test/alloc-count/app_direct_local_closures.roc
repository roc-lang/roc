app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A local closure that only its own body calls directly is called as its
# worker with its captures as arguments: building and calling it allocates
# nothing, whether it captures scalars, a refcounted value, or another such
# closure, calls itself, or is generalized and called at two types.
scaled : U64, U64 -> U64
scaled = |factor, offset| {
	scale = |k| k * factor + offset
	scale(1) + scale(2) + scale(3)
}

stepped : U64, U64 -> U64
stepped = |n, step| {
	# Annotated, so that it is not generic in its counter's number type,
	# whose arithmetic a generic body pays for on its own.
	count_down : U64, U64 -> U64
	count_down = |remaining, acc| if remaining == 0 acc else count_down(remaining - 1, acc + step)
	count_down(n, 0)
}

nested : U64, U64 -> U64
nested = |base, bonus| {
	inner = |x| x + base
	outer = |y| inner(y) * 2 + bonus
	outer(1) + outer(2)
}

lengths : List(U64), Str -> U64
lengths = |items, label| {
	measure = |extra| List.len(items) + Str.count_utf8_bytes(label) + extra
	measure(1) + measure(2)
}

generic_pick : List(a), a -> (a, a)
generic_pick = |items, fallback| {
	pick = |i| List.get(items, i) ?? fallback
	(pick(0), pick(1000))
}

two_types : U64, U8 -> U64
two_types = |amount, small| {
	echo = |x| (x, amount)
	(big, a) = echo(amount)
	(tiny, b) = echo(small)
	big + a + tiny.to_u64() + b
}

run! : Str => Str
run! = |input| {
	size = Str.count_utf8_bytes(input)
	items = List.repeat(size, 5)
	label = Str.concat("label", input)
	before = Host.alloc_count!()
	var $total = 0
	var $i = 0
	while $i < 100 {
		i = $i
		(picked, missing) = generic_pick(items, i)
		$total = $total + scaled(i, size) + stepped(i, 2) + nested(i, size) + lengths(items, label) + picked + missing + two_types(i, 3)
		$i = $i + 1
	}
	allocs = Host.alloc_count!() - before
	expect $total == 95200
	"total: ${$total.to_str()}, direct local closure allocations: ${allocs.to_str()}"
}
