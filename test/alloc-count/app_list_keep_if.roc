app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# List.keep_if and List.drop_if compact a uniquely owned list in place, so
# they allocate nothing. A shared list, and keep_oks, whose item type
# changes, build their result in one allocation. keep_oks allocates nothing
# when it keeps nothing. A shared list that keeps every item comes back as it is.
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

run! : Str => Str
run! = |input| {
	offset = Str.count_utf8_bytes(input)

	some = numbers(offset)
	some_before = Host.alloc_count!()
	some_kept = List.keep_if(some, |n| n - offset != 1 and n - offset != 4)
	some_allocs = Host.alloc_count!() - some_before
	expect some_kept == [offset, offset + 2, offset + 3, offset + 5, offset + 6, offset + 7]

	all = numbers(offset)
	all_before = Host.alloc_count!()
	all_kept = List.keep_if(all, |n| n >= offset)
	all_allocs = Host.alloc_count!() - all_before
	expect List.len(all_kept) == 8
	expect List.capacity(all_kept) == 8

	none = numbers(offset)
	none_before = Host.alloc_count!()
	none_kept = List.keep_if(none, |n| n < offset)
	none_allocs = Host.alloc_count!() - none_before
	expect none_kept == []
	expect List.capacity(none_kept) == 8

	dropped = numbers(offset)
	drop_before = Host.alloc_count!()
	drop_kept = List.drop_if(dropped, |n| n - offset < 6)
	drop_allocs = Host.alloc_count!() - drop_before
	expect drop_kept == [offset + 6, offset + 7]

	strs = words(offset)
	strs_before = Host.alloc_count!()
	strs_kept = List.keep_if(strs, |s| Str.count_utf8_bytes(s) % 2 == 0)
	strs_allocs = Host.alloc_count!() - strs_before
	expect List.len(strs_kept) == 2

	shared = numbers(offset)
	shared_before = Host.alloc_count!()
	shared_kept = List.keep_if(shared, |n| n - offset != 3)
	shared_allocs = Host.alloc_count!() - shared_before
	expect shared == numbers(offset)
	expect shared_kept == [offset, offset + 1, offset + 2, offset + 4, offset + 5, offset + 6, offset + 7]

	shared_all_before = Host.alloc_count!()
	shared_all_kept = List.keep_if(shared, |n| n >= offset)
	shared_all_allocs = Host.alloc_count!() - shared_all_before
	expect shared_all_kept == shared

	shared_none_before = Host.alloc_count!()
	shared_none_kept = List.keep_if(shared, |n| n < offset)
	shared_none_allocs = Host.alloc_count!() - shared_none_before
	expect shared_none_kept == []

	oks = numbers(offset)
	oks_before = Host.alloc_count!()
	oks_kept = List.keep_oks(
		oks,
		|n| if n - offset > 5 {
			Ok(n.to_str())
		} else {
			Err(n)
		},
	)
	oks_allocs = Host.alloc_count!() - oks_before
	expect List.len(oks_kept) == 2

	no_oks = numbers(offset)
	no_oks_before = Host.alloc_count!()
	no_oks_kept : List(Str)
	no_oks_kept = List.keep_oks(no_oks, |n| Err(n))
	no_oks_allocs = Host.alloc_count!() - no_oks_before
	expect no_oks_kept == []

	# --opt=speed drops expects, so the output reads `shared` after keep_if to
	# keep it shared there too.
	lengths = [some_kept, all_kept, none_kept, drop_kept, shared_kept, shared_all_kept, shared_none_kept].map(List.len)
	"keep_if allocations: some ${some_allocs.to_str()}, all ${all_allocs.to_str()}, none ${none_allocs.to_str()}, drop_if ${drop_allocs.to_str()}, strings ${strs_allocs.to_str()}, shared ${shared_allocs.to_str()}, shared all ${shared_all_allocs.to_str()}, shared none ${shared_none_allocs.to_str()}, keep_oks ${oks_allocs.to_str()}, keep_oks none ${no_oks_allocs.to_str()}\nkept lengths: ${Str.inspect(lengths)} ${List.len(strs_kept).to_str()} ${List.len(oks_kept).to_str()} ${List.len(no_oks_kept).to_str()}, shared input sum: ${List.sum(shared).to_str()}"
}
