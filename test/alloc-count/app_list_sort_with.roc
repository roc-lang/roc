app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12306
# Sorting a uniquely owned list with List.sort_with must not heap-allocate:
# the comparator is called directly rather than boxed, and a short list is
# sorted in place.
run! : Str => Str
run! = |input| {
	offset = Str.count_utf8_bytes(input)
	var $list = List.with_capacity(8)
	$list = List.append($list, offset + 5)
	$list = List.append($list, offset + 2)
	$list = List.append($list, offset + 7)
	$list = List.append($list, offset + 1)
	$list = List.append($list, offset + 8)
	$list = List.append($list, offset + 3)
	$list = List.append($list, offset + 6)
	$list = List.append($list, offset + 4)
	before = Host.alloc_count!()
	sorted = List.sort_with($list, |a, b| (a - offset).order_relative_to(b - offset))
	allocs = Host.alloc_count!() - before
	expect allocs == 0
	expect sorted == [offset + 1, offset + 2, offset + 3, offset + 4, offset + 5, offset + 6, offset + 7, offset + 8]
	"sorted length: ${List.len(sorted).to_str()}, sort allocations: ${allocs.to_str()}"
}
