ParserDictDuplicateKey :: [].{}

## A token format. `D n` opens a dict of `n` entries, read with no further
## calls to the format; `U` opens one that ends at a `.` token.
Tokens := [Default].{
	parse_str : Tokens, State -> Try({ value : Str, rest : State }, [Bad])
	parse_str = |_, state| take_token(state)

	parse_u64 : Tokens, State -> Try({ value : U64, rest : State }, [Bad])
	parse_u64 = |_, state| {
		parts = take_token(state)?

		match U64.from_str(parts.value) {
			Ok(value) => Ok({ value, rest: parts.rest })
			Err(_) => Err(Bad)
		}
	}

	parse_dict_start : Tokens, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad])
	parse_dict_start = |_, state| {
		head = take_token(state)?

		if Str.is_eq(head.value, "U") {
			return Ok(Uncounted(head.rest))
		}

		if !Str.is_eq(head.value, "D") {
			return Err(Bad)
		}

		count = take_token(head.rest)?

		match U64.from_str(count.value) {
			Ok(len) => Ok(Counted({ len, rest: count.rest }))
			Err(_) => Err(Bad)
		}
	}

	parse_dict_next : Tokens, State -> Try([Entry(State), Done(State)], [Bad])
	parse_dict_next = |_, state| {
		head = take_token(state)?

		if Str.is_eq(head.value, ".") Ok(Done(head.rest)) else Ok(Entry(state))
	}

	parse_dict_after_key : Tokens, State -> Try(State, [Bad])
	parse_dict_after_key = |_, state| Ok(state)

	parse_dict_after_entry : Tokens, State -> Try([Continue(State), Done(State)], [Bad])
	parse_dict_after_entry = |_, state| {
		head = take_token(state)?

		if Str.is_eq(head.value, ".") Ok(Done(head.rest)) else Ok(Continue(state))
	}

	parse_key_start : Tokens, State -> Try(State, [Bad])
	parse_key_start = |_, state| Ok(state)

	parse_key_str : Tokens, State -> Try({ value : Str, rest : State }, [Bad])
	parse_key_str = |_, state| take_token(state)

	parse_key_u64 : Tokens, State -> Try({ value : U64, rest : State }, [Bad])
	parse_key_u64 = |format, state| Tokens.parse_u64(format, state)

	invalid_value : Tokens, State -> [Bad]
	invalid_value = |_, _| Bad
}

State := { raw : Str }

take_token : State -> Try({ value : Str, rest : State }, [Bad])
take_token = |state| {
	trimmed = Str.trim_start(state.raw)

	if Str.is_empty(trimmed) {
		return Err(Bad)
	}

	match Str.split_first(trimmed, " ") {
		Ok({ before, after }) => Ok({ value: before, rest: State.{ raw: after } })
		Err(NotFound) => Ok({ value: trimmed, rest: State.{ raw: "" } })
	}
}

## A key whose equality ignores ASCII case, so two different spellings on the
## wire are one key to the dict.
Name :: Str.{
	parser_for : _
	is_eq : Name, Name -> Bool
	is_eq = |Name.(a), Name.(b)| Str.caseless_ascii_equals(a, b)
	to_hash : Name, Hasher -> Hasher
	to_hash = |Name.(text), hasher| Str.with_ascii_lowercased(text).to_hash(hasher)
}

spellings : Dict(Name, Str) -> List((Str, Str))
spellings = |dict| List.map(Dict.to_list(dict), |(Name.(text), value)| (text, value))

## Parse all of `raw`.
parse : Tokens, Str -> Try(a, [Bad])
	where [
		a.parser_for : Tokens -> (State -> Try({ value : a, rest : State }, [Bad])),
	]
parse = |format, raw| {
	Shape : a
	parse_shape = Shape.parser_for(format)
	parsed = parse_shape(State.{ raw })?

	if Str.is_empty(Str.trim_start(parsed.rest.raw)) Ok(parsed.value) else Err(Bad)
}

## Unequal keys each get an entry.
expect {
	result : Try(Dict(Str, Str), [Bad])
	result = parse(Tokens.Default, "D 3 first-key-long-enough-for-the-heap one-value-long-enough-for-the-heap second-key-long-enough-for-the-heap two-value-long-enough-for-the-heap third-key-long-enough-for-the-heap three-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([
		("first-key-long-enough-for-the-heap", "one-value-long-enough-for-the-heap"),
		("second-key-long-enough-for-the-heap", "two-value-long-enough-for-the-heap"),
		("third-key-long-enough-for-the-heap", "three-value-long-enough-for-the-heap"),
	])
}

expect {
	result : Try(Dict(U64, U64), [Bad])
	result = parse(Tokens.Default, "U 1 10 2 20 3 30 .")

	result.map_ok(Dict.to_list) == Ok([(1, 10), (2, 20), (3, 30)])
}

expect {
	result : Try(Dict(U64, U64), [Bad])
	result = parse(Tokens.Default, "D 0")

	result.map_ok(Dict.len) == Ok(0)
}

## A repeated key takes the value given last, wherever the collision falls,
## and the entry stays where the key first appeared.
expect {
	result : Try(Dict(Str, Str), [Bad])
	result = parse(Tokens.Default, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap b 3 c 4")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("b", "3"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad])
	result = parse(Tokens.Default, "D 4 a 1 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap c 4")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad])
	result = parse(Tokens.Default, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap b 2 c 3 repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("b", "2"),
		("c", "3"),
	])
}

## A key given three times keeps the third value.
expect {
	result : Try(Dict(U64, Str), [Bad])
	result = parse(Tokens.Default, "D 3 7 first-value-long-enough-for-the-heap 7 second-value-long-enough-for-the-heap 7 third-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([(7, "third-value-long-enough-for-the-heap")])
}

## A dict that ends at a terminator keeps the last value the same way.
expect {
	result : Try(Dict(Str, Str), [Bad])
	result = parse(Tokens.Default, "U a 1 b 2 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap .")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("b", "2"),
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
	])
}

## Keys that differ on the wire but are equal under the key type's `is_eq`
## collide, and the dict takes the last spelling together with its value.
expect {
	result : Try(Dict(Name, Str), [Bad])
	result = parse(Tokens.Default, "D 3 Content-Type-Header-Name text/plain-long-enough-for-the-heap accept */* content-type-header-name application/json-long-enough-for-the-heap")

	result.map_ok(spellings) == Ok([
		("content-type-header-name", "application/json-long-enough-for-the-heap"),
		("accept", "*/*"),
	])
}

## JSON keeps the value a repeated object name was given last.
expect {
	result : Try(Dict(Str, Str), [InvalidJson(Str)])
	result = Json.parse("{\"a\":\"first-value-long-enough-for-the-heap\",\"b\":\"2\",\"a\":\"second-value-long-enough-for-the-heap\"}")

	result.map_ok(Dict.to_list) == Ok([("a", "second-value-long-enough-for-the-heap"), ("b", "2")])
}
