# A converter without observations isolates portable successful-value reuse
# from the separate debug-order/failed-producer attribution contracts.
PlainValues :: [].{
	Quoted(a) := { text : Str }.{
		from_quote : Str -> Try(Quoted(a), [BadQuotedBytes(Str)])
		from_quote = |raw| Ok(Quoted.{ text: Str.concat(raw, "!") })
	}

	pair : U64 -> { left : Quoted(a), right : Quoted(a) }
	pair = |_n| { left: "left", right: "right" }

	make_i32 : U64 -> Str
	make_i32 = |n| {
		values : { left : Quoted(I32), right : Quoted(I32) }
		values = pair(n)
		Str.concat(values.left.text, values.right.text)
	}

	make_u64 : U64 -> Str
	make_u64 = |n| {
		values : { left : Quoted(U64), right : Quoted(U64) }
		values = pair(n)
		Str.concat(values.left.text, values.right.text)
	}

	plain : U64 -> U64
	plain = |n| n + 1
}
