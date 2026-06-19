ParserRuntimeRenameFields :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

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
				renamed = Fields.rename_fields(fields, |name|
					if Str.is_eq(name, "foo_bar") {
						"foo-bar"
					} else {
						name
					})

				match find_field(renamed, "foo-bar") {
					Ok(field) => Ok(Field({ field, rest: state }))
					Err(NotFound) => Ok(Done({ rest: state }))
				}
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

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser(Format.Default)
	parsed = parse_shape(State.Present(input))?
	Ok(parsed.value)
}

expect {
	result : Try({ foo_bar : Str }, [MissingRequired])
	result = parse("runtime-renamed")

	result == Ok({ foo_bar: "runtime-renamed" })
}
