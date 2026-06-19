ParserRenamedFieldBounds :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name|
		if Str.is_eq(name, "long_name") {
			"x"
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

				if Fields.shortest_name(fields) == 1 and Fields.longest_name(fields) == expected_longest {
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

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |_| Ok(Done)

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired

	missing_optional_field : Str, State -> [Missing]
	missing_optional_field = |_, _| Missing
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

find_any_field : Fields(_shape), Str -> Try(Field(_shape), [NotFound])
find_any_field = |fields, name| {
	var $remaining = Fields.iter(fields)

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

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

expect {
	result : Try({ long_name : Str }, [MissingRequired])
	result = parse("bounded")

	result == Ok({ long_name: "bounded" })
}

expect {
	result : Try({ foo : Try(Str, [Missing]), long_name : Str }, [MissingRequired])
	result = parse("bounded")

	result == Ok({ foo: Err(Missing), long_name: "bounded" })
}
