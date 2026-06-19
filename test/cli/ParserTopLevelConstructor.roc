ParserTopLevelConstructor :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name|
		if Str.is_eq(name, "foo_bar") {
			"foo-bar"
		} else {
			name
		}

	parse_str : State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(MissingRequired)
		}

	parse_record_field : Fields(_shape), State -> Try(
		[
			Field({ field : Field(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |fields, state|
		match state {
			Present(_) =>
				match find_field(fields, "foo-bar") {
					Ok(field) => Ok(Field({ field, rest: state }))
					Err(NotFound) => Ok(Done({ rest: state }))
				}

			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |_| Ok(Done)

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired
}

State := [Present(Str), Done]

find_field : Fields(_shape), Str -> Try(Field(_shape), [NotFound])
find_field = |fields, name| {
	var $remaining = Fields.for_size(fields, Str.count_utf8_bytes(name))

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if Str.is_eq(Field.name(item), name) {
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
