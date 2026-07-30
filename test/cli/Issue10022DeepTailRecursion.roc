# repro for https://github.com/roc-lang/roc/issues/10022
# A tail-recursive function must run in constant stack space in compiled code,
# including when a refcounted List is carried through every recursive call.
sum_bytes : List(U8), U64, U64 -> U64
sum_bytes = |bytes, index, acc|
	if index >= bytes.len() {
		acc
	} else {
		byte = bytes.get(index) ?? 0
		sum_bytes(bytes, index + 1, acc + byte.to_u64())
	}

main! = |args| {
	input = List.repeat(7, 1000000 + args.len())
	actual = sum_bytes(input, 0, 0)
	expected = input.len() * 7

	if actual != expected {
		crash "sum_bytes returned the wrong sum"
	}

	Ok({})
}
