HoledAnnotationRecursiveLiterals :: [].{}

# An annotated function with a `_` hole calls itself while its body is being
# checked. The hole is generalized with the body, so a numeral whose type the
# hole holds is a literal requirement of the function, and the recursive call
# passes the function's own literal argument on to itself.

go : U8 -> Try(U64, _)
go = |n| if n == 0 Err(Failed(1)) else go(n - 1)

scan : List(U8), List(Str) -> Try(List(Str), _)
scan = |input, seen| {
	len = input.fold(0, |acc, byte| acc * 256 + byte.to_u64())
	match input.first() {
		Ok('Z') => Ok(seen)
		Ok('E') => Err(Failed(len, 7))
		_ => scan(input.drop_first(1), seen.append("x"))
	}
}

small : Try(U64, [Failed(U8)])
small = go(3)

wide : Try(U64, [Failed(I64)])
wide = go(2)

expect small == Err(Failed(1))

expect wide == Err(Failed(1))

expect {
	fraction : Try(U64, [Failed(Dec)])
	fraction = go(0)
	fraction == Err(Failed(1.0))
}

expect scan(['A', 'B', 'Z'], []) == Ok(["x", "x"])

expect {
	failed : Try(List(Str), [Failed(U64, U8)])
	failed = scan(['A', 'E'], [])
	failed == Err(Failed(69, 7))
}
