# Generalized top-level callable values whose producers are general
# expressions. Boxy evaluates them inline at each use, at the use's
# instantiation of the binding's scheme (design.md "`.boxy` Runtime Lowering").
BoxyGeneralizedCallableValueUses :: [].{}

make_err : U64 -> Try(U64, [Oops(U64)])
make_err = {
	offset = 1
	|n| if n > 10 { Err(Oops(n + offset)) } else { Ok(n) }
}

wrap : a -> (U64 -> Try(a, [Oops(U64)]))
wrap = {
	scale = 2
	|value| |n| if n > 10 { Err(Oops(n * scale)) } else { Ok(value) }
}

# Uses `make_err` inside its own producer, so the inner use's caller type is
# written in `make_err2`'s quantified row.
make_err2 : U64 -> Try(U64, [Oops(U64)])
make_err2 = {
	base = make_err(50)
	|n| if n > 10 { base } else { Ok(n) }
}

# Calls `make_err` from the closure its producer returns.
make_err3 : U64 -> Try(U64, [Oops(U64)])
make_err3 = {
	k = 2
	|n| make_err(n * k)
}

forward : U64 -> Try(U64, [Oops(U64), Other(Str)])
forward = |n| {
	value = make_err(n)?
	if value == 3 { Err(Other("three")) } else { Ok(value) }
}

generic_use : U64 -> Try(U64, [Oops(U64), ..others])
generic_use = |n| make_err(n)

expect {
	a : Try(U64, [Oops(U64), Other])
	a = make_err(20)
	b : Try(U64, [Oops(U64), Third(Str)])
	b = make_err(5)
	a == Err(Oops(21)) and b == Ok(5)
}

expect forward(20) == Err(Oops(21)) and forward(3) == Err(Other("three"))

expect {
	c : Try(U64, [Oops(U64), Fourth])
	c = generic_use(30)
	c == Err(Oops(31))
}

expect {
	s : Try(Str, [Oops(U64)])
	s = wrap("hi")(4)
	l : Try(List(U64), [Oops(U64), Extra])
	l = wrap([1, 2])(40)
	s == Ok("hi") and l == Err(Oops(80))
}

expect {
	t : Try(U64, [Oops(U64), Fifth])
	t = make_err2(20)
	t == Err(Oops(51)) and make_err2(4) == Ok(4)
}

expect {
	u : Try(U64, [Oops(U64), Sixth])
	u = make_err3(20)
	u == Err(Oops(41)) and make_err3(4) == Ok(8)
}
