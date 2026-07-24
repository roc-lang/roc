ParserCountedFormat :: [].{}

## A length-prefixed format: every container declares how many entries it has,
## so the driver reads exactly that many with no further calls to the format.
## Values are whitespace-separated tokens; a container's count is the token
## right after its opening marker.
Counted := [Default].{
	rename_field : Counted, Str -> Str
	rename_field = |_, name| name

	parse_str : Counted, State -> Try({ value : Str, rest : State }, [Bad, ..])
	parse_str = |_, state| take_token(state)

	parse_u64 : Counted, State -> Try({ value : U64, rest : State }, [Bad, ..])
	parse_u64 = |_, state| {
		parts = take_token(state)?

		match U64.from_str(parts.value) {
			Ok(value) => Ok({ value, rest: parts.rest })
			Err(_) => Err(Bad)
		}
	}

	parse_list_start : Counted, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad, ..])
	parse_list_start = |_, state| take_count(state, "L")

	## Never reached: this format always reports a count up front.
	parse_list_next : Counted, State -> Try([Element(State), Done(State)], [Bad, ..])
	parse_list_next = |_, _| Err(Bad)

	parse_list_after_element : Counted, State -> Try([Continue(State), Done(State)], [Bad, ..])
	parse_list_after_element = |_, _| Err(Bad)

	parse_tuple_start : Counted, State, U64 -> Try(State, [Bad, ..])
	parse_tuple_start = |_, state, len| {
		counted = take_count(state, "T")?

		match counted {
			Counted({ len: declared, rest }) =>
				if declared == len {
					Ok(rest)
				} else {
					Err(Bad)
				}

			Uncounted(_) => Err(Bad)
		}
	}

	parse_tuple_next : Counted, State, U64, U64 -> Try(State, [Bad, ..])
	parse_tuple_next = |_, state, _, _| Ok(state)

	parse_tuple_end : Counted, State, U64 -> Try(State, [Bad, ..])
	parse_tuple_end = |_, state, _| Ok(state)

	parse_record_start : Counted, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad, ..])
	parse_record_start = |_, state| take_count(state, "R")

	parse_record_field : Counted,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[Bad, ..],
	)
	parse_record_field = |_, _, state| {
		parts = take_token(state)?
		Ok(TryField({ name: parts.value, rest: parts.rest }))
	}

	parse_record_after_field : Counted, State -> Try([Continue(State), Done(State)], [Bad, ..])
	parse_record_after_field = |_, _| Err(Bad)

	skip_record_field : Counted, State -> Try(State, [Bad, ..])
	skip_record_field = |_, state| {
		parts = take_token(state)?
		Ok(parts.rest)
	}

	parse_dict_start : Counted, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad, ..])
	parse_dict_start = |_, state| take_count(state, "D")

	parse_dict_next : Counted, State -> Try([Entry(State), Done(State)], [Bad, ..])
	parse_dict_next = |_, _| Err(Bad)

	parse_dict_after_key : Counted, State -> Try(State, [Bad, ..])
	parse_dict_after_key = |_, state| Ok(state)

	parse_dict_after_entry : Counted, State -> Try([Continue(State), Done(State)], [Bad, ..])
	parse_dict_after_entry = |_, _| Err(Bad)

	parse_key_str : Counted, State -> Try({ value : Str, rest : State }, [Bad, ..])
	parse_key_str = |_, state| take_token(state)

	parse_key_u64 : Counted, State -> Try({ value : U64, rest : State }, [Bad, ..])
	parse_key_u64 = |encoding, state| Counted.parse_u64(encoding, state)

	invalid_value : Counted, State -> [Bad, ..]
	invalid_value = |_, _| Bad
}

State := { raw : Str }

take_token : State -> Try({ value : Str, rest : State }, [Bad, ..])
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

## Consume `marker` followed by the entry count.
take_count : State, Str -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad, ..])
take_count = |state, marker| {
	head = take_token(state)?

	if !Str.is_eq(head.value, marker) {
		return Err(Bad)
	}

	count = take_token(head.rest)?

	match U64.from_str(count.value) {
		Ok(len) => Ok(Counted({ len, rest: count.rest }))
		Err(_) => Err(Bad)
	}
}

parse : Str -> Try(a, [Bad, ..errs])
	where [
		a.parser_for : Counted -> (State -> Try({ value : a, rest : State }, [Bad, ..errs])),
	]
parse = |raw| {
	Shape : a
	parse_shape = Shape.parser_for(Counted.Default)
	parsed = parse_shape(State.{ raw })?
	Ok(parsed.value)
}

expect {
	result : Try(List(U64), [Bad, MissingRequiredField(Str)])
	result = parse("L 3 10 20 30")

	result == Ok([10, 20, 30])
}

expect {
	result : Try(List(U64), [Bad, MissingRequiredField(Str)])
	result = parse("L 0")

	result == Ok([])
}

expect {
	result : Try(List(Str), [Bad, MissingRequiredField(Str)])
	result = parse("L 2 alpha beta")

	result == Ok(["alpha", "beta"])
}

expect {
	result : Try({ name : Str, age : U64 }, [Bad, MissingRequiredField(Str)])
	result = parse("R 2 name ada age 36")

	result == Ok({ name: "ada", age: 36 })
}

## A counted record stops at its declared count, so trailing input is left for
## the caller rather than being read as another field.
expect {
	result : Try({ name : Str }, [Bad, MissingRequiredField(Str)])
	result = parse("R 1 name ada age 36")

	result == Ok({ name: "ada" })
}

## An unknown field is skipped, and the skip consumes one of the declared
## entries rather than reading past the record.
expect {
	result : Try({ name : Str }, [Bad, MissingRequiredField(Str)])
	result = parse("R 2 zzz junk name ada trailing")

	result == Ok({ name: "ada" })
}

expect {
	result : Try({ name : Str }, [Bad, MissingRequiredField(Str)])
	result = parse("R 2 name first name second")

	result == Ok({ name: "second" })
}

## A counted record whose declared count runs out before a required field
## reports the field rather than reading past the count.
expect {
	result : Try({ name : Str, age : U64 }, [Bad, MissingRequiredField(Str)])
	result = parse("R 1 name ada age 36")

	result == Err(MissingRequiredField("age"))
}

expect {
	result : Try({ name : Try(Str, [Missing]) }, [Bad, MissingRequiredField(Str)])
	result = parse("R 0")

	result == Ok({ name: Err(Missing) })
}

expect {
	result : Try(Dict(Str, List(U64)), [Bad, MissingRequiredField(Str)])
	result = parse("D 2 a L 2 1 2 b L 0")

	match result {
		Ok(d) => Dict.get(d, "a") == Ok([1, 2]) and Dict.get(d, "b") == Ok([])
		Err(_) => False
	}
}

expect {
	result : Try(Dict(Str, U64), [Bad, MissingRequiredField(Str)])
	result = parse("D 2 a 1 b 2")

	match result {
		Ok(d) => Dict.len(d) == 2 and Dict.get(d, "a") == Ok(1) and Dict.get(d, "b") == Ok(2)
		Err(_) => False
	}
}

expect {
	result : Try(Dict(U64, Str), [Bad, MissingRequiredField(Str)])
	result = parse("D 1 7 seven")

	match result {
		Ok(d) => Dict.get(d, 7) == Ok("seven")
		Err(_) => False
	}
}

expect {
	result : Try((U64, Str), [Bad, MissingRequiredField(Str)])
	result = parse("T 2 9 nine")

	result == Ok((9, "nine"))
}

## The format sees the expected arity, so it rejects a tuple whose declared
## length disagrees before any element is read.
expect {
	result : Try((U64, Str), [Bad, MissingRequiredField(Str)])
	result = parse("T 3 9 nine ten")

	result == Err(Bad)
}

expect {
	result : Try(List(List(U64)), [Bad, MissingRequiredField(Str)])
	result = parse("L 2 L 2 1 2 L 1 3")

	result == Ok([[1, 2], [3]])
}
