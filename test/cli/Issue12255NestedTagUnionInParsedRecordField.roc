Issue12255NestedTagUnionInParsedRecordField :: [].{}

## repro for https://github.com/roc-lang/roc/issues/12255
## Parsed record fields whose types hold a tag union with a tag union in its
## payload, where the inner union's payload needs a format call of its own,
## check and parse like any other field.
Counted := [Default].{
	rename_field : Counted, Str -> Str
	rename_field = |_, name| name

	parse_str : Counted, State -> Try({ value : Str, rest : State }, [Bad])
	parse_str = |_, state| take_token(state)

	parse_list_start : Counted, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad])
	parse_list_start = |_, state| take_count(state, "L")

	parse_list_next : Counted, State -> Try([Item(State), Done(State)], [Bad])
	parse_list_next = |_, _| Err(Bad)

	parse_list_after_item : Counted, State -> Try([Continue(State), Done(State)], [Bad])
	parse_list_after_item = |_, _| Err(Bad)

	parse_record_start : Counted, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad])
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
		[Bad],
	)
	parse_record_field = |_, _, state| {
		parts = take_token(state)?
		Ok(TryField({ name: parts.value, rest: parts.rest }))
	}

	parse_record_after_field : Counted, State -> Try([Continue(State), Done(State)], [Bad])
	parse_record_after_field = |_, _| Err(Bad)

	skip_record_field : Counted, State -> Try(State, [Bad])
	skip_record_field = |_, state| {
		parts = take_token(state)?
		Ok(parts.rest)
	}

	parse_tag_union : Counted, Encoding.ParseTagUnionSpec(a), State -> Try({ value : a, rest : State }, [Bad])
	parse_tag_union = |encoding, spec, state| {
		parts = take_token(state)?

		Encoding.ParseTagUnionSpec.parse(
			spec,
			{
				tag: parts.value,
				encoding,
				state: parts.rest,
				start_payloads: |payload_state, _count| Ok(payload_state),
				next_payload: |payload_state, _index, _count| Ok(payload_state),
				finish_payloads: |payload_state, _count| Ok(payload_state),
				missing: Bad,
			},
		)
	}

	invalid_value : Counted, State -> [Bad]
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

take_count : State, Str -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Bad])
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

Wire : { steps : [Outer([Inner(List(Str))])] }

parse_wire : Str -> Try(Wire, [Bad, MissingRequiredField(Str)])
parse_wire = |text| {
	wire : Wire
	wire = parse(text)?
	Ok(wire)
}

expect parse_wire("R 1 steps Outer Inner L 2 a b") == Ok({ steps: Outer(Inner(["a", "b"])) })

RecordPayloadWire : { steps : [Outer([Inner({ a : Str })])] }

parse_record_payload_wire : Str -> Try(RecordPayloadWire, [Bad, MissingRequiredField(Str)])
parse_record_payload_wire = |text| {
	wire : RecordPayloadWire
	wire = parse(text)?
	Ok(wire)
}

expect parse_record_payload_wire("R 1 steps Outer Inner R 1 a x") == Ok({ steps: Outer(Inner({ a: "x" })) })

TryWire : { steps : Try([Inner(List(Str))], [Missing]) }

parse_try_wire : Str -> Try(TryWire, [Bad, MissingRequiredField(Str)])
parse_try_wire = |text| {
	wire : TryWire
	wire = parse(text)?
	Ok(wire)
}

expect parse_try_wire("R 1 steps Inner L 2 a b") == Ok({ steps: Ok(Inner(["a", "b"])) })
