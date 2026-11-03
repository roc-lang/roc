NestedOptionalFieldPattern :: [].{}

# A record pattern means the same thing nested as on its own: destructuring an
# optional field binds `Try(value, [MissingField])` however deep the pattern
# sits, in a statement or in a parameter.

two_step : { outer : { n ?: Str } } -> Try(Str, [MissingField])
two_step = |r| {
	{ outer } = r
	{ n } = outer
	n
}

nested : { outer : { n ?: Str } } -> Try(Str, [MissingField])
nested = |r| {
	{ outer: { n } } = r
	n
}

in_parameter : { outer : { n ?: Str, k : U8 } } -> (Try(Str, [MissingField]), U8)
in_parameter = |{ outer: { n, k } }| (n, k)

deeper : { a : { b : { c ?: U8 }, d : U8 } } -> U8
deeper = |r| {
	{ a: { b: { c }, d } } = r
	match c {
		Ok(value) => value + d
		Err(MissingField) => d
	}
}

heap = "hi there, a heap-sized string"

expect two_step({ outer: { n: heap } }) == nested({ outer: { n: heap } })
expect (match nested({ outer: { n: heap } }) { Ok(s) => s == heap, Err(MissingField) => Bool.False })
expect (match nested({ outer: {} }) { Ok(_) => Bool.False, Err(MissingField) => Bool.True })
expect (match in_parameter({ outer: { k: 3 } }) { (Err(MissingField), 3) => Bool.True, _ => Bool.False })
expect deeper({ a: { b: { c: 4 }, d: 5 } }) == 9
expect deeper({ a: { b: {}, d: 5 } }) == 5
