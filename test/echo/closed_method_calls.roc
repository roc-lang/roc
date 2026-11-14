# Method calls on builtin types whose methods take and return only types with
# no arguments, at top level, in functions, and in a function that works for
# any type that has the method.

greeting : Str
greeting = "hello".concat(", ").concat("world")

shout : Str -> Str
shout = |word| word.trim().concat("!").repeat(2)

describe : U64 -> Str
describe = |n| n.to_str().concat(" bytes")

any_concat = |a, b| a.concat(b)

main! = |args| {
	# A runtime count, so the calls below run when the program does.
	count = args.len()
	sized = describe(greeting.count_utf8_bytes() + count * 0)
	blank = "".repeat(count)
	empty = if blank.is_empty() and greeting.starts_with("hello") "yes" else "no"
	echo!("${greeting} | ${shout("  hey ")} | ${sized} | ${empty} | ${any_concat("a", "b")}")
	Ok({})
}
