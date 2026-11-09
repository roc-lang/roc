Issue12264LowlevelTryWidenedError :: [].{}

# repro for https://github.com/roc-lang/roc/issues/12264: a low-level builtin's
# Try error keeps its own tag when it flows into a wider error union, even when
# the wider union has a tag whose name sorts before the builtin's tag.
parse : Str -> Try(I64, [BadNumStr, AboveLimit])
parse = |s| {
	n = I64.from_str(s)?
	if n > 100 { Err(AboveLimit) } else { Ok(n) }
}

narrow : I64 -> Try(U8, [OutOfRange, BadText])
narrow = |n| n.to_u8_try()

# The builtin's error tag carries a payload, whose position also moves.
decode : List(U8) -> Try(Str, [Aaa(U64), BadUtf8({ problem : Str.Utf8Problem, index : U64 })])
decode = |bytes| Str.from_utf8(bytes)

decode_index : List(U8) -> U64
decode_index = |bytes|
	match decode(bytes) {
		Ok(_) => 1000
		Err(Aaa(n)) => n
		Err(BadUtf8({ index, .. })) => index
	}

# The builtin used as a function value, and called inside a callback.
apply : a, (a -> Try(b, err)) -> Try(b, err)
apply = |x, f| f(x)

parse_value : Str -> Try(I64, [AboveLimit, BadNumStr])
parse_value = |s| apply(s, I64.from_str)

narrow_all : List(I64) -> Try(List(U8), [Aaa, OutOfRange])
narrow_all = |list| list.map_try(|n| n.to_u8_try())

# A builtin's non-Try result row widens the same way.
order : I64, I64 -> [Aaa, After, Before, Same]
order = |a, b| a.order_relative_to(b)

expect parse("5") == Ok(5)
expect parse("500") == Err(AboveLimit)
expect parse("x") == Err(BadNumStr)
expect parse("x") != Err(AboveLimit)
expect narrow(7) == Ok(7)
expect narrow(300) == Err(OutOfRange)
expect narrow(300) != Err(BadText)
expect decode_index([97, 255]) == 1
expect decode([104, 105]) == Ok("hi")
expect parse_value("x") == Err(BadNumStr)
expect parse_value("12") == Ok(12)
expect narrow_all([1, 2]) == Ok([1, 2])
expect narrow_all([1, 300]) == Err(OutOfRange)
expect order(1, 2) == Before
expect order(2, 2) == Same
expect order(3, 2) == After
