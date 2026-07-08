ParserTopLevelConstructor :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name|
		if Str.is_eq(name, "foo_bar") {
			"foo-bar"
		} else {
			name
		}

	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(MissingRequired)
		}

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, fields, state|
		match state {
			Present(_) =>
				match find_field(fields, "foo-bar") {
					Ok(field) => Ok(Field({ field, rest: state }))
					Err(NotFound) => Ok(Done({ rest: state }))
				}

			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired
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

Shape : { foo_bar : Str }

parse_shape : State -> Try({ value : { foo_bar : Str }, rest : State }, [MissingRequired])
parse_shape = Shape.parser_for(Format.Default)

expect {
	parsed = parse_shape(State.Present("from top level"))?

	parsed.value == { foo_bar: "from top level" }
}
