StoredForwardedRecordParser :: [].{}

# A generic builder derives the parser of a record of its type variable from
# that variable's own `parser_for` requirement, and returns the parser. The
# stored constants keep that parser as a compile-time function value, built at
# `Str`, and the format is defined here, so every parse goes through it.

Format := [Default].{
	missing_field : Format, Str -> [FormatError]
	missing_field = |_, _| FormatError

	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[FormatError],
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done(state))
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

make_parser : () -> (State -> Try({ value : { foo : a }, rest : State }, [FormatError]))
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError])),
	]
make_parser = || {
	Shape : { foo : a }
	Shape.parser_for(Format.Default)
}

parse_stored : State -> Try({ value : { foo : Str }, rest : State }, [FormatError])
parse_stored = make_parser()

# The builder's variable is itself a variable of a generic caller, whose own
# requirement supplies the field's parser.
relay : () -> (State -> Try({ value : { foo : b }, rest : State }, [FormatError]))
	where [
		b.parser_for : Format -> (State -> Try({ value : b, rest : State }, [FormatError])),
	]
relay = || make_parser()

parse_relayed : State -> Try({ value : { foo : Str }, rest : State }, [FormatError])
parse_relayed = relay()

expect {
	result = parse_stored(State.Present("stored"))?

	result.value == { foo: "stored" }
}

expect {
	parse_fresh : State -> Try({ value : { foo : Str }, rest : State }, [FormatError])
	parse_fresh = make_parser()
	result = parse_fresh(State.Present("fresh"))?

	result.value == { foo: "fresh" }
}

expect {
	result = parse_relayed(State.Present("relayed"))?

	result.value == { foo: "relayed" }
}
