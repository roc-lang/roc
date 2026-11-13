app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# repro for https://github.com/roc-lang/roc/issues/12680
#
# List.keep_if_try and List.keep_if_try! allocate like List.keep_if: they
# compact a uniquely owned list in place, so they allocate nothing, even when
# the predicate stops early with Err. A shared list builds its result in one
# allocation, and a shared list that keeps every item comes back as it is.
numbers : U64 -> List(U64)
numbers = |offset| {
	var $list = List.with_capacity(8)
	var $i = 0
	while $i < 8 {
		$list = List.append($list, offset + $i)
		$i = $i + 1
	}
	$list
}

words : U64 -> List(Str)
words = |offset| {
	var $list = List.with_capacity(4)
	var $i = 0
	while $i < 4 {
		$list = List.append($list, Str.repeat("a word long enough for the heap", offset + $i))
		$i = $i + 1
	}
	$list
}

kept_len : Try(List(a), [Stop]) -> U64
kept_len = |result| match result {
	Ok(list) => List.len(list)
	Err(Stop) => 99
}

run! : Str => Str
run! = |input| {
	offset = Str.count_utf8_bytes(input)

	some = numbers(offset)
	some_before = Host.alloc_count!()
	some_kept = List.keep_if_try(some, |n| Ok(n - offset != 1 and n - offset != 4))
	some_allocs = Host.alloc_count!() - some_before
	expect some_kept == Ok([offset, offset + 2, offset + 3, offset + 5, offset + 6, offset + 7])

	all = numbers(offset)
	all_before = Host.alloc_count!()
	all_kept = List.keep_if_try(all, |n| Ok(n >= offset))
	all_allocs = Host.alloc_count!() - all_before
	expect kept_len(all_kept) == 8

	none = numbers(offset)
	none_before = Host.alloc_count!()
	none_kept = List.keep_if_try(none, |n| Ok(n < offset))
	none_allocs = Host.alloc_count!() - none_before
	expect none_kept == Ok([])

	strs = words(offset)
	strs_before = Host.alloc_count!()
	strs_kept = List.keep_if_try(strs, |s| Ok(Str.count_utf8_bytes(s) % 2 == 0))
	strs_allocs = Host.alloc_count!() - strs_before
	expect kept_len(strs_kept) == 2

	stopped = numbers(offset)
	stopped_before = Host.alloc_count!()
	stopped_kept = List.keep_if_try(
		stopped,
		|n| if n - offset == 5 {
			Err(Stop)
		} else {
			Ok(n - offset != 1)
		},
	)
	stopped_allocs = Host.alloc_count!() - stopped_before
	expect stopped_kept == Err(Stop)

	stopped_strs = words(offset)
	stopped_strs_before = Host.alloc_count!()
	stopped_strs_kept = List.keep_if_try(
		stopped_strs,
		# The first word is dropped, so the kept ones after it are swapped
		# down before the last word stops the pass.
		|s| if Str.count_utf8_bytes(s) > 31 * (offset + 2) {
			Err(Stop)
		} else {
			Ok(Str.count_utf8_bytes(s) > 31 * offset)
		},
	)
	stopped_strs_allocs = Host.alloc_count!() - stopped_strs_before
	expect stopped_strs_kept == Err(Stop)

	effectful = numbers(offset)
	effectful_before = Host.alloc_count!()
	effectful_kept = List.keep_if_try!(effectful, |n| Ok(n - offset > 2))
	effectful_allocs = Host.alloc_count!() - effectful_before
	expect effectful_kept == Ok([offset + 3, offset + 4, offset + 5, offset + 6, offset + 7])

	shared = numbers(offset)
	shared_before = Host.alloc_count!()
	shared_kept = List.keep_if_try(shared, |n| Ok(n - offset != 3))
	shared_allocs = Host.alloc_count!() - shared_before
	expect shared == numbers(offset)
	expect shared_kept == Ok([offset, offset + 1, offset + 2, offset + 4, offset + 5, offset + 6, offset + 7])

	shared_all_before = Host.alloc_count!()
	shared_all_kept = List.keep_if_try(shared, |n| Ok(n >= offset))
	shared_all_allocs = Host.alloc_count!() - shared_all_before
	expect shared_all_kept == Ok(shared)

	shared_effectful_before = Host.alloc_count!()
	shared_effectful_kept = List.keep_if_try!(shared, |n| Ok(n - offset < 2))
	shared_effectful_allocs = Host.alloc_count!() - shared_effectful_before
	expect shared_effectful_kept == Ok([offset, offset + 1])

	# --opt=speed drops expects, so the output reads every result, and reads
	# `shared` after keep_if_try to keep it shared there too.
	lengths = [some_kept, all_kept, none_kept, stopped_kept, effectful_kept, shared_kept, shared_all_kept, shared_effectful_kept].map(kept_len)
	"keep_if_try allocations: some ${some_allocs.to_str()}, all ${all_allocs.to_str()}, none ${none_allocs.to_str()}, strings ${strs_allocs.to_str()}, stopped ${stopped_allocs.to_str()}, stopped strings ${stopped_strs_allocs.to_str()}, effectful ${effectful_allocs.to_str()}, shared ${shared_allocs.to_str()}, shared all ${shared_all_allocs.to_str()}, shared effectful ${shared_effectful_allocs.to_str()};\nkept lengths: ${Str.inspect(lengths)} ${kept_len(strs_kept).to_str()} ${kept_len(stopped_strs_kept).to_str()}, shared input sum: ${List.sum(shared).to_str()};"
}
