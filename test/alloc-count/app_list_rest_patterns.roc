app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# List rest patterns over lists of refcounted items. A rest from the front of
# a uniquely owned list releases the items behind it right away, so `inner`,
# dropped that way, is uniquely owned again and grows in place without
# allocating. Rests that skip leading items, and rests of a list that stays
# live, give the right items.
drop_back : List(List(U8)) -> List(List(U8))
drop_back = |items| match items {
	[.. as rest, _] => rest
	[] => []
}

drop_front : List(List(U8)) -> List(List(U8))
drop_front = |items| match items {
	[_, .. as rest] => rest
	[] => []
}

middle : List(List(U8)) -> List(List(U8))
middle = |items| match items {
	[_, .. as rest, _] => rest
	_ => []
}

run! : Str => Str
run! = |input| {
	seed = Str.count_utf8_bytes(input).to_u8_wrap()
	inner = List.with_capacity(8).append(seed)
	outer = [[1.U8], [2, 2], inner]
	rest = drop_back(outer)
	before = Host.alloc_count!()
	grown = inner.append(9)
	grow_allocs = Host.alloc_count!() - before
	released = !rest.contains(grown) and rest.len() == 2

	shared = [[seed + 1], [3, 3, 3], [4, 4, 4, 4], [5]]
	front = drop_front(shared)
	inside = middle(shared)
	back = drop_back(shared)

	"rest released: ${Str.inspect(released)}, grow allocations: ${grow_allocs.to_str()}, rest lengths: ${Str.inspect(front.map(List.len))} ${Str.inspect(inside.map(List.len))} ${Str.inspect(back.map(List.len))}, source: ${shared.len().to_str()}"
}
