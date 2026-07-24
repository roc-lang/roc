ParserRenamedFieldBounds :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name|
		if Str.is_eq(name, "long_name") {
			"x"
		} else {
			name
		}

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_, state|
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
	parse_record_field = |_, fields, state|
		match state {
			Present(_) => {
				has_foo = match find_any_field(fields, "foo") {
					Ok(_) => Bool.True
					Err(NotFound) => Bool.False
				}
				expected_longest = if has_foo {
					3
				} else {
					1
				}

				if Encoding.FieldName.FieldNames.shortest_name(fields) == 1 and Encoding.FieldName.FieldNames.longest_name(fields) == expected_longest {
					match find_field(fields, "x") {
						Ok(field) => Ok(Field({ field, rest: state }))
						Err(NotFound) => Ok(Done({ rest: state }))
					}
				} else {
					Ok(Done({ rest: state }))
				}
			}

			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

find_field : Encoding.FieldName.FieldNames(_shape), Str -> Try(Encoding.FieldName(_shape), [NotFound])
find_field = |fields, name| {
	var $remaining = Encoding.FieldName.FieldNames.for_size(fields, Str.count_utf8_bytes(name))

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if Str.is_eq(Encoding.FieldName.name(item), name) {
					return Ok(item)
				} else {
					$remaining = rest
				}

			Skip({ rest }) => {
				$remaining = rest
			}

			Done =>
				return Err(NotFound)
			}
	}
}

find_any_field : Encoding.FieldName.FieldNames(_shape), Str -> Try(Encoding.FieldName(_shape), [NotFound])
find_any_field = |fields, name| {
	var $remaining = Encoding.FieldName.FieldNames.iter(fields)

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if Str.is_eq(Encoding.FieldName.name(item), name) {
					return Ok(item)
				} else {
					$remaining = rest
				}

			Skip({ rest }) => {
				$remaining = rest
			}

			Done =>
				return Err(NotFound)
			}
	}
}

parse : Str -> Try(a, [FormatError, ..errs])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [FormatError, ..errs])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

expect {
	result : Try({ long_name : Str }, [FormatError, MissingRequiredField(Str)])
	result = parse("bounded")

	result == Ok({ long_name: "bounded" })
}

expect {
	result : Try({ foo : Try(Str, [Missing]), long_name : Str }, [FormatError, MissingRequiredField(Str)])
	result = parse("bounded")

	result == Ok({ foo: Err(Missing), long_name: "bounded" })
}
