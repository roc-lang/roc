ParserWildcardOptionalField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_u64 : Format, State -> Try({ value : U64, rest : State }, [FormatError, ..])
	parse_u64 = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[FormatError, ..],
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "count", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(U64), Done]

parse : State -> Try(a, [FormatError, ..errs])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

# A `Try(ok, _)` field's wildcard error row pins to the generic `[Missing]`
# optional-field marker.
expect {
	present : Try({ count : Try(U64, _) }, [FormatError])
	present = parse(State.Present(42))

	absent : Try({ count : Try(U64, _) }, [FormatError])
	absent = parse(State.Done)

	(present == Ok({ count: Ok(42) })) and (absent == Ok({ count: Err(Missing) }))
}
