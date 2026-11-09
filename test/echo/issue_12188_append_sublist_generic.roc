# Appending a sublist to an empty list whose elements the generic
# `List.append_sublist` stores behind a descriptor.
app [main!] {}

main! = |_args| {
	_ = [].append_sublist([1.U8], { start: 0, len: 1 })
	Ok({})
}
