main! = |args| {
	value = mk(List.len(args), 2)
	echo!("${Str.inspect(value)}\n")
	echo!("${Str.inspect({ inner: value })}\n")
	echo!("${Str.inspect([value, mk(3, 4)])}\n")
	Ok({})
}

Loose := [L(U64, U64)].{
	to_inspect : Loose -> Str
	to_inspect = |L(_)| "custom"
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)
