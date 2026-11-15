app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A sublist returned through a value join takes its input's ownership, so a
# sublist from the front of a uniquely owned list releases the items behind
# it right away: `inner`, dropped that way, is uniquely owned again and grows
# in place without allocating. Lending the input and retaining the returned
# sublist would keep the dropped items alive inside the source allocation.
drop_back : List(List(U8)) -> List(List(U8))
drop_back = |items| match items.len() {
	0 => []
	n => {
		rest = List.sublist(items, { start: 0, len: n - 1 })
		rest
	}
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
	back = drop_back(shared)

	"rest released: ${Str.inspect(released)}, grow allocations: ${grow_allocs.to_str()}, rest length: ${back.len().to_str()}, source: ${shared.len().to_str()}"
}
