CodecSchemeRowsPerUse :: [].{}

import CodecSchemeRowsPerUseLib

# One unannotated codec definition, used at several error rows in one body.
# Each call owns its row, on every lowering path.

Format := [Default].{
	parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, [Negative])
	parse_u64 = |_, state| if state == 7 {
		Err(Negative)
	} else {
		Ok({ value: state, rest: state + 1 })
	}

	encode_u64 : U64, U64 -> Try(U64, [Overflow])
	encode_u64 = |value, state| if value == 7 {
		Err(Overflow)
	} else {
		Ok(state + value)
	}
}

Generic := [Default].{
	parse_u64 : Generic, state -> Try({ value : U64, rest : state }, [Negative])
	parse_u64 = |_, state| Ok({ value: 1, rest: state })
}

Wide : [Negative, Reset(Str)]

Parsed(row) : Try({ value : U64, rest : U64 }, row)

show_narrow : Parsed([Negative]) -> Str
show_narrow = |result|
	match result {
		Ok(parsed) => parsed.value.to_str()
		Err(Negative) => "negative"
	}

show_wide : Parsed(Wide) -> Str
show_wide = |result|
	match result {
		Ok(parsed) => parsed.value.to_str()
		Err(Negative) => "negative"
		Err(Reset(message)) => message
	}

any_parser = |_| U64.parser_for(Format.Default)

forwarding = |unit| any_parser(unit)

any_encoder = |_| U64.encoder_for(Format.Default)

any_state = |_| U64.parser_for(Generic.Default)

expect {
	narrow : U64 -> Parsed([Negative])
	narrow = any_parser({})
	wide : U64 -> Parsed(Wide)
	wide = any_parser({})
	(show_narrow(narrow(7)), show_wide(wide(7)), show_wide(wide(3))) == ("negative", "negative", "3")
}

expect {
	narrow : U64 -> Parsed([Negative])
	narrow = forwarding({})
	wide : U64 -> Parsed(Wide)
	wide = forwarding({})
	(show_narrow(narrow(7)), show_wide(wide(3))) == ("negative", "3")
}

expect {
	first = any_parser({})
	second = any_parser({})
	state : U64
	state = 7
	a = match first(state) {
		Ok(_) => "ok"
		Err(Negative) => "negative"
	}
	b = match second(state) {
		Ok(_) => "ok"
		Err(Negative) => "negative"
		Err(Other) => "other"
	}
	(a, b) == ("negative", "negative")
}

expect {
	narrow : { parse : U64 -> Parsed([Negative]) }
	narrow = { parse: any_parser({}) }
	wide : { parse : U64 -> Parsed(Wide) }
	wide = { parse: any_parser({}) }
	(show_narrow((narrow.parse)(7)), show_wide((wide.parse)(3))) == ("negative", "3")
}

expect {
	narrow : U64, U64 -> Try(U64, [Overflow])
	narrow = any_encoder({})
	wide : U64, U64 -> Try(U64, [Overflow, Reset(Str)])
	wide = any_encoder({})
	first = match narrow(7, 2) {
		Ok(sum) => sum
		Err(Overflow) => 100
	}
	second = match wide(1, 2) {
		Ok(sum) => sum
		Err(Overflow) => 100
		Err(Reset(_)) => 200
	}
	(first, second) == (100, 3)
}

expect {
	numbers : U64 -> Parsed([Negative])
	numbers = any_state({})
	text : Str -> Try({ value : U64, rest : Str }, Wide)
	text = any_state({})
	second = match text("x") {
		Ok(parsed) => parsed.rest
		Err(_) => "error"
	}
	(show_narrow(numbers(7)), second) == ("1", "x")
}

expect {
	narrow : U64 -> Parsed([Negative])
	narrow = CodecSchemeRowsPerUseLib.any_parser({})
	wide : U64 -> Parsed(Wide)
	wide = CodecSchemeRowsPerUseLib.any_parser({})
	(show_narrow(narrow(7)), show_wide(wide(3))) == ("negative", "3")
}

expect {
	alias = any_parser
	narrow : U64 -> Parsed([Negative])
	narrow = alias({})
	wide : U64 -> Parsed(Wide)
	wide = alias({})
	(show_narrow(narrow(7)), show_wide(wide(3))) == ("negative", "3")
}

# A binding nothing uses leaves the relation it copied as a template.
expect {
	_ = any_parser
	1 == 1
}

unused_local = |state| {
	_local = |_| U64.parser_for(Format.Default)(state)
	1
}

expect {
	state : U64
	state = 3
	unused_local(state) == 1
}

known_state = |state| {
	typed : U64
	typed = state
	U64.parser_for(Format.Default)(typed)
}

expect {
	state : U64
	state = 7
	narrow : Parsed([Negative])
	narrow = known_state(state)
	wide : Parsed(Wide)
	wide = known_state(state)
	(show_narrow(narrow), show_wide(wide)) == ("negative", "negative")
}
