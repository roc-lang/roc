app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for issue 11822. A record chosen by an `if`/`match` value (including
# `?`) must hand its list fields on uniquely, so appending to them in a loop
# stays O(log n) in allocations instead of copying the list every iteration.
Buffers : { items : List(U64), other : List(U64) }

push : Buffers, U64 -> Try(Buffers, [Full])
push = |buffers, item| {
	{ items, other } = buffers
	if items.len() > 1000000000 {
		Err(Full)
	} else {
		Ok({ items: items.append(item), other })
	}
}

push_plain : Buffers, U64 -> Buffers
push_plain = |buffers, item| {
	{ items, other } = buffers
	{ items: items.append(item), other }
}

question : U64 -> Try(U64, [Full])
question = |n| {
	var $buffers = { items: [], other: [] }
	var $index = 0
	while $index < n {
		pushed = push($buffers, $index)?
		$buffers = { ..pushed, items: pushed.items.append($index) }
		$index = $index + 1
	}
	Ok($buffers.items.len())
}

if_value : U64 -> U64
if_value = |n| {
	var $buffers = { items: [], other: [] }
	var $index = 0
	while $index < n {
		pushed = if $index == n + 1 {
			push_plain($buffers, 0)
		} else {
			push_plain($buffers, $index)
		}
		$buffers = { ..pushed, items: pushed.items.append($index) }
		$index = $index + 1
	}
	$buffers.items.len()
}

plain : U64 -> U64
plain = |n| {
	var $buffers = { items: [], other: [] }
	var $index = 0
	while $index < n {
		pushed = push_plain($buffers, $index)
		$buffers = { ..pushed, items: pushed.items.append($index) }
		$index = $index + 1
	}
	$buffers.items.len()
}

run! : Str => Str
run! = |input| {
	n = Str.count_utf8_bytes(input) * 64

	question_before = Host.alloc_count!()
	question_len = match question(n) {
		Ok(len) => len
		Err(Full) => 0
	}
	question_allocs = Host.alloc_count!() - question_before

	if_before = Host.alloc_count!()
	if_len = if_value(n)
	if_allocs = Host.alloc_count!() - if_before

	plain_before = Host.alloc_count!()
	plain_len = plain(n)
	plain_allocs = Host.alloc_count!() - plain_before

	"lengths: ${question_len.to_str()} ${if_len.to_str()} ${plain_len.to_str()}, allocations: ${question_allocs.to_str()} ${if_allocs.to_str()} ${plain_allocs.to_str()}"
}
