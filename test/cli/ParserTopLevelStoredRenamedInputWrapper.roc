ParserTopLevelStoredRenamedInputWrapper :: [].{}

Format := [Default].{
	rename_field : Str -> Str
	rename_field = |name| snake_to_camel(name)

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
				match find_field(fields, "fooBar") {
					Ok(field) => Ok(Field({ field, rest: state }))
					Err(NotFound) => Ok(TryField({ name: "fooBar", rest: state }))
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

snake_to_camel : Str -> Str
snake_to_camel = |text|
	match Str.find_first(text, "_") {
		Ok({ before, after }) =>
			before.concat(upper_first_ascii(snake_to_camel(after)))

		Err(NotFound) => text
	}

upper_first_ascii : Str -> Str
upper_first_ascii = |text| {
	bytes = Str.to_utf8(text)

	match List.first(bytes) {
		Ok(first) => {
			upper = if first >= 97 {
				if first <= 122 {
					first - 32
				} else {
					first
				}
			} else {
				first
			}

			match Str.from_utf8([upper].concat(List.drop_first(bytes, 1))) {
				Ok(value) => value
				Err(_) => text
			}
		}

		Err(_) => text
	}
}

parser : () -> (Str -> Try(a, [MissingRequired]))
	where [
		a.parser : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parser = || {
	Shape : a
	parse_shape = Shape.parser(Format.Default)

	|input| {
		parsed = parse_shape(State.Present(input))?
		Ok(parsed.value)
	}
}

parse_stored : Str -> Try({ foo_bar : Str }, [MissingRequired])
parse_stored = parser()

expect {
	result = parse_stored("stored")?

	result == { foo_bar: "stored" }
}
