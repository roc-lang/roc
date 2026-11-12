app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12931
# A string completed at compile time keeps the capacity it was evaluated with.
# Reading it allocates nothing, and filling it to that capacity allocates once,
# as it would had the program built the string itself.
empty_reserved : Str
empty_reserved = Str.with_capacity(1000)

partly_filled : Str
partly_filled = Str.with_capacity(500).concat("ab")

in_record : { text : Str, count : U64 }
in_record = { text: Str.with_capacity(300), count: 3 }

run! : Str => Str
run! = |input| {
	piece = if Str.count_utf8_bytes(input) > 1000 "y" else "x"
	start = Host.alloc_count!()

	total = Str.count_utf8_bytes(empty_reserved) + Str.count_utf8_bytes(partly_filled) + Str.count_utf8_bytes(in_record.text)
	read_allocs = Host.alloc_count!() - start

	var $first = empty_reserved
	var $i = 0
	while $i < 1000 {
		$first = Str.concat($first, piece)
		$i = $i + 1
	}
	first_allocs = Host.alloc_count!() - start - read_allocs

	var $second = partly_filled
	var $j = 0
	while $j < 498 {
		$second = Str.concat($second, piece)
		$j = $j + 1
	}
	second_allocs = Host.alloc_count!() - start - read_allocs - first_allocs

	var $third = in_record.text
	var $k = 0
	while $k < 300 {
		$third = Str.concat($third, piece)
		$k = $k + 1
	}
	third_allocs = Host.alloc_count!() - start - read_allocs - first_allocs - second_allocs

	reserved = Str.reserve(partly_filled, 100).concat(piece)
	reserved_allocs = Host.alloc_count!() - start - read_allocs - first_allocs - second_allocs - third_allocs

	expect total == 2
	expect read_allocs == 0
	expect first_allocs == 1
	expect second_allocs == 1
	expect third_allocs == 1
	expect reserved_allocs == 1
	expect Str.starts_with($second, "abx")
	expect partly_filled == "ab"
	expect reserved == "abx"
	"read: ${total.to_str()}, lengths: ${Str.count_utf8_bytes($first).to_str()} ${Str.count_utf8_bytes($second).to_str()} ${Str.count_utf8_bytes($third).to_str()}, allocations: ${read_allocs.to_str()} ${first_allocs.to_str()} ${second_allocs.to_str()} ${third_allocs.to_str()} ${reserved_allocs.to_str()}"
}
