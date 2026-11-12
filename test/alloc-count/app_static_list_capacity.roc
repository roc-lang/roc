app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12931
# A list completed at compile time keeps the capacity it was evaluated with.
# Reading it allocates nothing, and filling it to that capacity allocates once,
# as it would had the program built the list itself.
partly_filled : List(U64)
partly_filled = List.with_capacity(1000).append(1).append(2)

in_record : { buffer : List(Str), count : U64 }
in_record = { buffer: List.with_capacity(300), count: 3 }

# One capture-free function is a zero-sized item where this is evaluated, and
# an item with a tag where `run!` appends a second function. The reservation
# is recorded as a count there and allocated here.
functions : List(U64 -> U64)
functions = List.with_capacity(50).append(|x| x + 1)

run! : Str => Str
run! = |input| {
	extra = Str.count_utf8_bytes(input)
	start = Host.alloc_count!()

	total = List.len(partly_filled) + List.len(in_record.buffer) + List.len(functions)
	read_allocs = Host.alloc_count!() - start

	var $numbers = partly_filled
	var $i = 0
	while $i < 998 {
		$numbers = List.append($numbers, $i + extra)
		$i = $i + 1
	}
	numbers_allocs = Host.alloc_count!() - start - read_allocs

	var $strings = in_record.buffer
	var $j = 0
	while $j < 300 {
		$strings = List.append($strings, "x")
		$j = $j + 1
	}
	strings_allocs = Host.alloc_count!() - start - read_allocs - numbers_allocs

	var $functions = functions
	var $k = 0
	while $k < 49 {
		$functions = List.append($functions, |x| x + 2)
		$k = $k + 1
	}
	functions_allocs = Host.alloc_count!() - start - read_allocs - numbers_allocs - strings_allocs

	var $cleared = List.clear(partly_filled)
	var $m = 0
	while $m < 1000 {
		$cleared = List.append($cleared, $m + extra)
		$m = $m + 1
	}
	cleared_allocs = Host.alloc_count!() - start - read_allocs - numbers_allocs - strings_allocs - functions_allocs

	expect total == 3
	expect read_allocs == 0
	expect numbers_allocs == 1
	expect strings_allocs == 1
	expect functions_allocs == 1
	expect cleared_allocs == 1
	expect List.first($numbers) == Ok(1)
	expect List.len(partly_filled) == 2
	"lengths: ${List.len($numbers).to_str()} ${List.len($strings).to_str()} ${List.len($functions).to_str()} ${List.len($cleared).to_str()}, allocations: ${read_allocs.to_str()} ${numbers_allocs.to_str()} ${strings_allocs.to_str()} ${functions_allocs.to_str()} ${cleared_allocs.to_str()}"
}
