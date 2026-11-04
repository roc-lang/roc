# Unannotated helpers whose numerals sit at generic types. The `0` in
# `contains_private` reaches `remove` only through the call's constraint
# callable, so `remove` holds the requirement as evidence of its own and
# every call between the two passes exactly what the callee declares.
remove = |set, private_id| {
	bit = 1.U64.shl_wrap(private_id % 64)
	mask = bit.bitwise_not()
	if !contains_private(set, private_id) { set } else {
		match private_id / 64 {
			0 => { word0: set.word0.bitwise_and(mask), word1: set.word1, word2: set.word2, length: set.length - 1 }
			1 => { word0: set.word0, word1: set.word1.bitwise_and(mask), word2: set.word2, length: set.length - 1 }
			_ => { word0: set.word0, word1: set.word1, word2: set.word2.bitwise_and(mask), length: set.length - 1 }
		}
	}
}

contains_private = |set, private_id| {
	bit = 1.U64.shl_wrap(private_id % 64)
	word = match private_id / 64 {
		0 => set.word0
		1 => set.word1
		_ => set.word2
	}
	word.bitwise_and(bit) != 0
}

main! = |args| {
	n = List.len(args).to_u8_wrap()
	set = { word0: 3.U64, word1: 0.U64, word2: 0.U64, length: 2.U8 }
	removed = remove(set, n)
	kept = remove(removed, n)
	echo!("${Str.inspect(removed.length)} ${Str.inspect(removed.word0)} ${Str.inspect(kept.length)} ${Str.inspect(kept.word0)}")
	Ok({})
}
