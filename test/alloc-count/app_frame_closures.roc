app [run!] { pf: platform "./platform/main.roc" }

import pf.Host
import FrameCallee

# A capturing closure passed only to parameters that never let it escape is
# built in its caller's frame (design.md "Frame Callables"), so each call below
# allocates nothing: the list operations work in place on unique lists, and
# the closures are not boxed, including the comparator `List.sort_with`
# captures in a closure of its own. A closure passed to `Try.on_err`, whose
# callback is called by a deferred tail call, is still a heap allocation.
numbers : U64 -> List(U64)
numbers = |offset| {
	var $list = List.with_capacity(8)
	var $i = 0
	while $i < 8 {
		$list = List.append($list, offset + 8 - $i)
		$i = $i + 1
	}
	$list
}

run! : Str => Str
run! = |input| {
	offset = Str.count_utf8_bytes(input)

	keep = numbers(offset)
	keep_before = Host.alloc_count!()
	kept = List.keep_if(keep, |n| n - offset != 3)
	keep_allocs = Host.alloc_count!() - keep_before
	expect List.len(kept) == 7

	sort = numbers(offset)
	sort_before = Host.alloc_count!()
	sorted = List.sort_with(sort, |a, b| (a - offset).order_relative_to(b - offset))
	sort_allocs = Host.alloc_count!() - sort_before
	expect List.first(sorted) == Ok(offset + 1)

	var $dict = Dict.with_capacity(16)
	$dict = Dict.insert($dict, 1, offset)
	update_before = Host.alloc_count!()
	$dict = Dict.update($dict, 1, |r| Ok((r ?? 0) + offset))
	update_allocs = Host.alloc_count!() - update_before
	expect Dict.get($dict, 1) == Ok(2 * offset)

	contains = numbers(offset)
	contains_before = Host.alloc_count!()
	found = List.contains(contains, offset + 4)
	contains_allocs = Host.alloc_count!() - contains_before
	expect found

	cross_before = Host.alloc_count!()
	both = FrameCallee.apply_both(|n| n + offset, 1)
	cross_allocs = Host.alloc_count!() - cross_before
	expect both == 3 + 2 * offset

	failed : Try(U64, U64)
	failed = Err(offset)
	on_err_before = Host.alloc_count!()
	recovered = Try.on_err(failed, |e| Ok(e + offset))
	on_err_allocs = Host.alloc_count!() - on_err_before
	expect recovered == Ok(2 * offset)

	sum = List.sum(kept) + List.sum(sorted) + List.sum(contains) + both
	"frame closures: keep_if ${keep_allocs.to_str()}, sort_with ${sort_allocs.to_str()}, Dict.update ${update_allocs.to_str()}, List.contains ${contains_allocs.to_str()}, cross-module ${cross_allocs.to_str()}, Try.on_err ${on_err_allocs.to_str()}, sum ${sum.to_str()}"
}
