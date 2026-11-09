app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# List.keep_while and List.keep_until return a prefix of their input, so they
# allocate nothing for a unique or a shared list, wherever they stop.
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
	some_kept = List.keep_while(some, |n| n - offset < 3)
	some_allocs = Host.alloc_count!() - some_before
	expect some_kept == [offset, offset + 1, offset + 2]

	none = numbers(offset)
	none_before = Host.alloc_count!()
	none_kept = List.keep_while(none, |n| n < offset)
	none_allocs = Host.alloc_count!() - none_before
	expect none_kept == []

	all = numbers(offset)
	all_before = Host.alloc_count!()
	all_kept = List.keep_while(all, |n| n >= offset)
	all_allocs = Host.alloc_count!() - all_before
	expect List.len(all_kept) == 8

	until = numbers(offset)
	until_before = Host.alloc_count!()
	until_kept = List.keep_until(until, |n| n - offset == 3)
	until_allocs = Host.alloc_count!() - until_before
	expect until_kept == [offset, offset + 1, offset + 2]

	shared = numbers(offset)
	shared_before = Host.alloc_count!()
	shared_kept = List.keep_while(shared, |n| n - offset < 3)
	shared_allocs = Host.alloc_count!() - shared_before
	expect shared_kept == [offset, offset + 1, offset + 2]

	shared_all_before = Host.alloc_count!()
	shared_all_kept = List.keep_while(shared, |n| n >= offset)
	shared_all_allocs = Host.alloc_count!() - shared_all_before
	expect shared_all_kept == shared

	strs = words(offset)
	strs_before = Host.alloc_count!()
	strs_kept = List.keep_while(strs, |word| word.count_utf8_bytes() < 31 * (offset + 2))
	strs_allocs = Host.alloc_count!() - strs_before
	expect List.len(strs_kept) == 2

	# --opt=speed drops expects, so the output reads `shared` after keep_while to
	# keep it shared there too.
	lengths = [some_kept, none_kept, all_kept, until_kept, shared_kept, shared_all_kept].map(List.len)
	"keep_while allocations: some ${some_allocs.to_str()}, none ${none_allocs.to_str()}, all ${all_allocs.to_str()}, keep_until ${until_allocs.to_str()}, shared ${shared_allocs.to_str()}, shared all ${shared_all_allocs.to_str()}, strings ${strs_allocs.to_str()}\nkept lengths: ${Str.inspect(lengths)} ${List.len(strs_kept).to_str()}, shared input sum: ${List.sum(shared).to_str()}"
}
