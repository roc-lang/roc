# Cross-engine conformance for constant aggregates: every executor must agree
# on lists that compile-time evaluation freezes into static data (packed
# scalars, a list of lists sharing one row, strings, records holding
# strings, distinct heap strings) and on a repeat whose count is a literal
# but whose element is only known at runtime. The static lists of strings
# pass through functions that retain, release, and write to them, so a
# frozen header reaching the allocator fails the run.

scalars = List.repeat(3.U64, 7000)

nested = List.repeat(List.repeat(1.U64, 260), 300)

words = List.repeat("a string too long to be stored inline", 3000)

table = ["alpha is longer than a small string", "beta is longer than a small string", "gamma"]

records = List.repeat({ name: "a record name longer than a small string", n: 7.U32 }, 500)

keep_first : List(Str), U64 -> List(Str)
keep_first = |strs, count| List.take_first(strs, count)

with_extra : List(Str), Str -> List(Str)
with_extra = |strs, extra| List.append(strs, extra)

total_bytes : List(Str) -> U64
total_bytes = |strs| strs.fold(0, |acc, s| acc + Str.to_utf8(s).len())

main! = |args| {
	n = args.len()
	runtime = List.repeat(n + 5, 4000)

	scalar_sum = scalars.fold(0, |acc, x| acc + x)
	nested_sum = nested.fold(0, |acc, row| acc + row.fold(0, |a, x| a + x))
	runtime_sum = runtime.fold(0, |acc, x| acc + x)
	kept = keep_first(words, 2 + n)
	extended = with_extra(table, "delta is longer than a small string too")
	replaced = List.set(table, n, "a replacement string longer than small") ?? table
	record_sum = records.fold(0, |acc, r| acc + r.n.to_u64() + Str.to_utf8(r.name).len())

	echo!("scalars: ${scalars.len().to_str()} ${scalar_sum.to_str()}\n")
	echo!("nested: ${nested.len().to_str()} ${nested_sum.to_str()}\n")
	echo!("runtime: ${runtime.len().to_str()} ${runtime_sum.to_str()}\n")
	echo!("words: ${words.len().to_str()} ${total_bytes(words).to_str()} ${kept.len().to_str()}\n")
	echo!("table: ${Str.inspect(extended)}\n")
	echo!("replaced: ${Str.inspect(replaced)}\n")
	echo!("original: ${Str.inspect(table)}\n")
	echo!("records: ${records.len().to_str()} ${record_sum.to_str()}\n")
	Ok({})
}
