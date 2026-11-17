ParserDictDuplicateKey :: [].{}

## A token format whose encoding value is its answer to a dict entry whose key
## the dict already holds. `D n` opens a dict of `n` entries, read with no
## further calls to the format; `U` opens one that ends at a `.` token.
Policy := [First, Last, Refuse].{
	parse_str : Policy, State -> Try({ value : Str, rest : State }, [Bad, Duplicate])
	parse_str = |_, state| take_token(state)

	parse_u64 : Policy, State -> Try({ value : U64, rest : State }, [Bad, Duplicate])
	parse_u64 = |_, state| {
		parts = take_token(state)?

		match U64.from_str(parts.value) {
			Ok(value) => Ok({ value, rest: parts.rest })
			Err(_) => Err(Bad)
		}
	}

	parse_dict_start : Policy, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad, Duplicate])
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

	parse_dict_next : Policy, State -> Try([Entry(State), Done(State)], [Bad, Duplicate])
	parse_dict_next = |_, state| {
		head = take_token(state)?

		if Str.is_eq(head.value, ".") Ok(Done(head.rest)) else Ok(Entry(state))
	}

	parse_dict_after_key : Policy, State -> Try(State, [Bad, Duplicate])
	parse_dict_after_key = |_, state| Ok(state)

	parse_dict_after_entry : Policy, State -> Try([Continue(State), Done(State)], [Bad, Duplicate])
	parse_dict_after_entry = |_, state| {
		head = take_token(state)?

		if Str.is_eq(head.value, ".") Ok(Done(head.rest)) else Ok(Continue(state))
	}

	parse_dict_duplicate_key : Policy, State -> Try([KeepFirst(State), KeepLast(State)], [Bad, Duplicate])
	parse_dict_duplicate_key = |policy, state|
		match policy {
			First => Ok(KeepFirst(state))
			Last => Ok(KeepLast(state))
			Refuse => Err(Duplicate)
		}

	parse_key_start : Policy, State -> Try(State, [Bad, Duplicate])
	parse_key_start = |_, state| Ok(state)

	parse_key_str : Policy, State -> Try({ value : Str, rest : State }, [Bad, Duplicate])
	parse_key_str = |_, state| take_token(state)

	parse_key_u64 : Policy, State -> Try({ value : U64, rest : State }, [Bad, Duplicate])
	parse_key_u64 = |policy, state| Policy.parse_u64(policy, state)

	invalid_value : Policy, State -> [Bad, Duplicate]
	invalid_value = |_, _| Bad
}

State := { raw : Str }

take_token : State -> Try({ value : Str, rest : State }, [Bad, Duplicate])
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

## Parse all of `raw` under `policy`.
parse : Policy, Str -> Try(a, [Bad, Duplicate])
	where [
		a.parser_for : Policy -> (State -> Try({ value : a, rest : State }, [Bad, Duplicate])),
	]
parse = |policy, raw| {
	Shape : a
	parse_shape = Shape.parser_for(policy)
	parsed = parse_shape(State.{ raw })?

	if Str.is_empty(Str.trim_start(parsed.rest.raw)) Ok(parsed.value) else Err(Bad)
}

## Unequal keys never reach the format's duplicate-key method, whatever its
## answer would have been.
expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 3 first-key-long-enough-for-the-heap one-value-long-enough-for-the-heap second-key-long-enough-for-the-heap two-value-long-enough-for-the-heap third-key-long-enough-for-the-heap three-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([
		("first-key-long-enough-for-the-heap", "one-value-long-enough-for-the-heap"),
		("second-key-long-enough-for-the-heap", "two-value-long-enough-for-the-heap"),
		("third-key-long-enough-for-the-heap", "three-value-long-enough-for-the-heap"),
	])
}

expect {
	result : Try(Dict(U64, U64), [Bad, Duplicate])
	result = parse(Policy.Refuse, "U 1 10 2 20 3 30 .")

	result.map_ok(Dict.to_list) == Ok([(1, 10), (2, 20), (3, 30)])
}

expect {
	result : Try(Dict(U64, U64), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 0")

	result.map_ok(Dict.len) == Ok(0)
}

## KeepFirst drops the later entry, wherever the collision falls, and parsing
## carries on from the state after the dropped value.
expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.First, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap b 3 c 4")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "first-value-long-enough-for-the-heap"),
		("b", "3"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.First, "D 4 a 1 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap c 4")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("repeated-key-long-enough-for-the-heap", "first-value-long-enough-for-the-heap"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.First, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap b 2 c 3 repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "first-value-long-enough-for-the-heap"),
		("b", "2"),
		("c", "3"),
	])
}

## KeepLast replaces the held value and leaves the entry where it was.
expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap b 3 c 4")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("b", "3"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "D 4 a 1 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap c 4")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap b 2 c 3 repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
		("b", "2"),
		("c", "3"),
	])
}

## A format that refuses a repeated key fails the whole parse with its own
## error, wherever the collision falls.
expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap b 3 c 4")

	result.map_ok(Dict.to_list) == Err(Duplicate)
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 4 a 1 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap c 4")

	result.map_ok(Dict.to_list) == Err(Duplicate)
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 4 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap b 2 c 3 repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Err(Duplicate)
}

## A key held three times asks the format twice.
expect {
	result : Try(Dict(U64, Str), [Bad, Duplicate])
	result = parse(Policy.First, "D 3 7 first-value-long-enough-for-the-heap 7 second-value-long-enough-for-the-heap 7 third-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([(7, "first-value-long-enough-for-the-heap")])
}

expect {
	result : Try(Dict(U64, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "D 3 7 first-value-long-enough-for-the-heap 7 second-value-long-enough-for-the-heap 7 third-value-long-enough-for-the-heap")

	result.map_ok(Dict.to_list) == Ok([(7, "third-value-long-enough-for-the-heap")])
}

## A dict that ends at a terminator settles repeated keys the same way.
expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.First, "U a 1 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap c 4 .")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("repeated-key-long-enough-for-the-heap", "first-value-long-enough-for-the-heap"),
		("c", "4"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "U a 1 b 2 repeated-key-long-enough-for-the-heap first-value-long-enough-for-the-heap repeated-key-long-enough-for-the-heap second-value-long-enough-for-the-heap .")

	result.map_ok(Dict.to_list) == Ok([
		("a", "1"),
		("b", "2"),
		("repeated-key-long-enough-for-the-heap", "second-value-long-enough-for-the-heap"),
	])
}

expect {
	result : Try(Dict(Str, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "U a 1 b 2 a 3 .")

	result.map_ok(Dict.to_list) == Err(Duplicate)
}

## Keys that differ on the wire but are equal under the key type's `is_eq`
## collide: KeepFirst holds on to the first spelling and value, and KeepLast
## takes the last spelling together with its value.
expect {
	result : Try(Dict(Name, Str), [Bad, Duplicate])
	result = parse(Policy.First, "D 3 Content-Type-Header-Name text/plain-long-enough-for-the-heap accept */* content-type-header-name application/json-long-enough-for-the-heap")

	result.map_ok(spellings) == Ok([
		("Content-Type-Header-Name", "text/plain-long-enough-for-the-heap"),
		("accept", "*/*"),
	])
}

expect {
	result : Try(Dict(Name, Str), [Bad, Duplicate])
	result = parse(Policy.Last, "D 3 Content-Type-Header-Name text/plain-long-enough-for-the-heap accept */* content-type-header-name application/json-long-enough-for-the-heap")

	result.map_ok(spellings) == Ok([
		("content-type-header-name", "application/json-long-enough-for-the-heap"),
		("accept", "*/*"),
	])
}

expect {
	result : Try(Dict(Name, Str), [Bad, Duplicate])
	result = parse(Policy.Refuse, "D 3 Content-Type-Header-Name text/plain-long-enough-for-the-heap accept */* content-type-header-name application/json-long-enough-for-the-heap")

	result.map_ok(spellings) == Err(Duplicate)
}

## JSON keeps the value a repeated object name was given last.
expect {
	result : Try(Dict(Str, Str), [InvalidJson(Str)])
	result = Json.parse("{\"a\":\"first-value-long-enough-for-the-heap\",\"b\":\"2\",\"a\":\"second-value-long-enough-for-the-heap\"}")

	result.map_ok(Dict.to_list) == Ok([("a", "second-value-long-enough-for-the-heap"), ("b", "2")])
}
