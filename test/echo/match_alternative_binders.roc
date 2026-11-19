# Each branch below lists alternatives that bind the same names from
# different patterns. The constants are evaluated while compiling; `main!`
# repeats each match at run time on values built from the argument count.

Shape : [Pair(Str, U64), Swapped(U64, Str), Boxed({ name : Str, n : U64 }), Single(Str), Nested(Try(Str, Str)), Empty]

describe : Shape -> Str
describe = |shape| match shape {
	Pair(name, n) | Swapped(n, name) | Boxed({ name, n }) => "${name}:${n.to_str()}"
	Single(name) | Nested(Ok(name)) | Nested(Err(name)) => name
	Empty => "empty"
}

guarded : Shape -> Str
guarded = |shape| match shape {
	Pair(name, n) | Swapped(n, name) if n > 5 => "big ${name}"
	Pair(name, _) | Swapped(_, name) => "small ${name}"
	_ => "other"
}

rest_sum : [A(List(U64)), B(Str, List(U64))] -> U64
rest_sum = |value| match value {
	A([first, .. as rest]) | B(_, [first, .. as rest]) => first + List.len(rest)
	A([]) | B(_, []) => 0
}

either = |result| match result {
	Ok(v) | Err(v) => v
}

first_ok = {
	value = if True Ok(3) else Err(4)
	match value {
		Ok(v) | Err(v) => v
	}
}

later_err = {
	value = if False Ok(3) else Err(4)
	match value {
		Ok(v) | Err(v) => v
	}
}

constants = [
	describe(Pair("a", 1)),
	describe(Swapped(2, "b")),
	describe(Boxed({ name: "c", n: 3 })),
	describe(Nested(Err("d"))),
	guarded(Swapped(9, "e")),
	guarded(Pair("f", 1)),
	rest_sum(B("x", [10, 20, 30])).to_str(),
	either(if False Ok("g") else Err("h")),
	Str.inspect(either(if True Ok({ n: 1.U8, s: "i" }) else Err({ n: 2.U8, s: "j" }))),
	Str.inspect((first_ok, later_err)),
]

main! = |args| {
	n = List.len(args)
	echo!(Str.join_with(constants, " "))
	runtime = [
		describe(if n > 100 Pair("x", 1) else Swapped(n + 2, "k")),
		describe(if n > 100 Single("x") else Nested(Ok("l"))),
		describe(if n > 100 Single("x") else Boxed({ name: "m", n })),
		guarded(if n > 100 Pair("x", 1) else Pair("n", n + 10)),
		rest_sum(if n > 100 A([]) else A([n + 4, 5])).to_str(),
		either(if n > 100 Ok("x") else Err("o")),
		Str.inspect(either(if n > 100 Ok([1.U8]) else Err([n.to_u8_wrap(), 7]))),
		Str.inspect(
			{
				value = if n > 100 Ok(n) else Err(n + 40)
				match value {
					Ok(v) | Err(v) => v
				}
			},
		),
	]
	echo!(Str.join_with(runtime, " "))
	Ok({})
}
