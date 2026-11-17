BoxyDirectLocalClosures := {}

# Local functions and closures that only their own bodies call directly.
# Under `--specialize=no` each is called as its worker, its captures passed
# as arguments, and no function value is built for it. One closure below is
# also used as a value, which keeps its function value for that use.

scaled_sum : U64, U64 -> U64
scaled_sum = |factor, offset| {
	scale = |k| k * factor + offset
	scale(1) + scale(2) + scale(3)
}

expect scaled_sum(10, 1) == 63

labelled : Str -> (Str, Str)
labelled = |label| {
	wrap = |x| { value: x, label }
	number = wrap(7.U64)
	text = wrap("seven")
	("${number.label}:${number.value.to_str()}", "${text.label}:${text.value}")
}

expect labelled("n") == ("n:7", "n:seven")

stepped_count : U64, U64 -> U64
stepped_count = |n, step| {
	count_down = |remaining, acc| if remaining == 0 acc else count_down(remaining - 1, acc + step)
	count_down(n, 0)
}

expect stepped_count(100000, 3) == 300000

stepped_sum : U64, U64 -> U64
stepped_sum = |n, step| {
	sum_to = |k| if k == 0 0 else k * step + sum_to(k - 1)
	sum_to(n)
}

expect stepped_sum(10, 2) == 110

nested : U64, U64 -> U64
nested = |base, bonus| {
	inner = |x| x + base
	outer = |y| inner(y) * 2 + bonus
	outer(1) + outer(2) + inner(100)
}

expect nested(10, 5) == 22 + 5 + 24 + 5 + 110

mixed : U64 -> (U64, List(U64))
mixed = |amount| {
	add = |x| x + amount
	(add(1), List.map([1, 2, 3], add))
}

expect mixed(10) == (11, [11, 12, 13])

first_or : List(a), a -> (a, a)
first_or = |items, fallback| {
	pick = |i| List.get(items, i) ?? fallback
	(pick(0), pick(1000))
}

expect first_or(["a", "b"], "z") == ("a", "z")
expect first_or([5.U64], 9) == (5, 9)

keeps_captured_list : List(Str) -> (U64, U64, List(Str))
keeps_captured_list = |words| {
	extended = |extra| List.append(words, extra)
	first = extended("x")
	second = extended("y")
	(List.len(first), List.len(second), words)
}

expect keeps_captured_list(["a", "b"]) == (3, 3, ["a", "b"])

called_from_value : List(U64), U64 -> List(U64)
called_from_value = |items, base| {
	helper = |x| x + base
	List.map(items, |y| helper(y) * 2)
}

expect called_from_value([1, 2], 3) == [8, 10]

in_nested_body : List(U64), U64 -> List(U64)
in_nested_body = |items, base| List.map(
	items,
	|y| {
		shift = |z| z + y + base
		shift(1) + shift(2)
	},
)

expect in_nested_body([1, 10], 100) == [205, 223]

padded : U64 -> (List(U64), List(Str))
padded = |count| {
	fill = |x, n, acc| if n == count acc else fill(x, n + 1, List.append(acc, x))
	(fill(7.U64, 0, []), fill("s", 0, []))
}

expect padded(3) == ([7, 7, 7], ["s", "s", "s"])
expect padded(0) == ([], [])
