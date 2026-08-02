# Every annotation-only call-site intrinsic must lower identically through
# method syntax and its qualified Builtin call.
IntrinsicMethodDispatch :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError, ..])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError, ..])
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
		[FormatError, ..],
	)
	parse_record_field = |_, fields, state|
		match state {
			Present(_) => {
				renamed = fields.rename_fields(|name|
					if Str.is_eq(name, "foo_bar") "foo-bar" else name)

				if renamed.shortest_name() != 7 {
					Err(FormatError)
				} else if renamed.longest_name() != 7 {
					Err(FormatError)
				} else {
					match find_any_field(renamed) {
						Err(_) => Err(FormatError)
						Ok(first) =>
							if !Str.is_eq(first.name(), "foo-bar") {
								Err(FormatError)
							} else {
								match find_field(renamed, "foo-bar") {
									Ok(field) => Ok(Field({ field, rest: state }))
									Err(_) => Err(FormatError)
								}
							}
					}
				}
			}

			Done => Ok(Done(state))
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError, ..])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

find_field : Encoding.FieldName.FieldNames(_shape), Str -> Try(Encoding.FieldName(_shape), [FormatError])
find_field = |fields, name| {
	var $remaining = fields.for_size(Str.count_utf8_bytes(name))

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if Str.is_eq(item.name(), name) {
					return Ok(item)
				} else {
					$remaining = rest
				}

			Skip({ rest }) => {
				$remaining = rest
			}

			Done => return Err(FormatError)
		}
	}
}

find_any_field : Encoding.FieldName.FieldNames(_shape) -> Try(Encoding.FieldName(_shape), [FormatError])
find_any_field = |fields| {
	var $remaining = fields.iter()

	while True {
		match Iter.next($remaining) {
			One({ item, .. }) => return Ok(item)
			Skip({ rest }) => {
				$remaining = rest
			}
			Done => return Err(FormatError)
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
	result : Try({ foo_bar : Str }, [FormatError, MissingRequiredField(Str)])
	result = parse("method-dispatch")

	result == Ok({ foo_bar: "method-dispatch" })
}
