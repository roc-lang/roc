ParserStoredAndRuntimePreparedFields :: [].{}

Format := [Identity, Kebab].{
	rename_field : Format, Str -> Str
	rename_field = |format, name|
		match format {
			Identity => name
			Kebab => underscores_to_dashes(name)
		}

	parse_str : State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |state|
		match state {
			KebabFooValue => Ok({ value: "foo-kebab", rest: KebabDone })
			IdentityFooValue => Ok({ value: "foo-identity", rest: IdentityDone })
			KebabFooName | KebabDone | IdentityFooName | IdentityDone | Done => Err(MissingRequired)
		}

	parse_record_field : Fields(_shape),
	State -> Try(
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
			KebabFooName => emit_field(fields, "foo-bar", KebabFooValue, KebabDone)
			KebabDone => Ok(Done({ rest: Done }))
			IdentityFooName => emit_field(fields, "foo_bar", IdentityFooValue, IdentityDone)
			IdentityDone => Ok(Done({ rest: Done }))
			KebabFooValue | IdentityFooValue | Done => Ok(Done({ rest: Done }))
		}

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |state|
		match state {
			KebabFooName => Ok(KebabDone)
			IdentityFooName => Ok(IdentityDone)
			KebabFooValue | KebabDone | IdentityFooValue | IdentityDone | Done => Ok(Done)
		}

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired

	missing_optional_field : Str, State -> [Missing]
	missing_optional_field = |_, _| Missing
}

State := [
	KebabFooName,
	KebabFooValue,
	KebabDone,
	IdentityFooName,
	IdentityFooValue,
	IdentityDone,
	Done,
]

Shape : {
	foo_bar : Str,
	maybe_token : Try(Str, [Missing]),
}

emit_field : Fields(_shape),
Str,
State,
State -> Try(
	[
		Field({ field : Field(_shape), rest : State }),
		TryField({ name : Str, rest : State }),
		TryFieldCaseless({ name : Str, rest : State }),
		Continue({ rest : State }),
		Done({ rest : State }),
	],
	[MissingRequired],
)
emit_field = |fields, name, value_state, next_state| {
	name_len = Str.count_utf8_bytes(name)

	if name_len < Fields.shortest_name(fields) {
		Ok(Continue({ rest: next_state }))
	} else if name_len > Fields.longest_name(fields) {
		Ok(Continue({ rest: next_state }))
	} else {
		match find_field(fields, name) {
			Ok(field) => Ok(Field({ field, rest: value_state }))
			Err(NotFound) => Ok(Continue({ rest: next_state }))
		}
	}
}

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

parse_stored_kebab : State -> Try({ value : Shape, rest : State }, [MissingRequired])
parse_stored_kebab = Shape.parser_for(Format.Kebab)

parse_stored_identity : State -> Try({ value : Shape, rest : State }, [MissingRequired])
parse_stored_identity = Shape.parser_for(Format.Identity)

parse_with_format : Format, State -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse_with_format = |format, input| {
	Parsed : a
	parse_shape = Parsed.parser_for(format)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

parse_runtime : Format, State -> Try(Shape, [MissingRequired])
parse_runtime = |format, input| {
	parsed : Shape
	parsed = parse_with_format(format, input)?

	Ok(parsed)
}

parse_runtime_from_flag : Bool, State -> Try(Shape, [MissingRequired])
parse_runtime_from_flag = |use_kebab, input| {
	format = if use_kebab {
		Format.Kebab
	} else {
		Format.Identity
	}

	parsed : Shape
	parsed = parse_with_format(format, input)?

	Ok(parsed)
}

expect {
	parsed = parse_stored_kebab(KebabFooName)?

	shape_is(parsed.value, "foo-kebab")
}

expect {
	parsed = parse_stored_identity(IdentityFooName)?

	shape_is(parsed.value, "foo-identity")
}

expect {
	result = parse_runtime(Format.Kebab, KebabFooName)?

	shape_is(result, "foo-kebab")
}

expect {
	result = parse_runtime(Format.Identity, IdentityFooName)?

	shape_is(result, "foo-identity")
}

expect {
	result = parse_runtime_from_flag(Bool.True, KebabFooName)?

	result.foo_bar == "foo-kebab"
}

underscores_to_dashes : Str -> Str
underscores_to_dashes = |text|
	match text.find_first("_") {
		Ok({ before, after }) =>
			before.concat("-").concat(underscores_to_dashes(after))

		Err(NotFound) => text
	}

is_missing : Try(Str, [Missing]) -> Bool
is_missing = |value|
	match value {
		Ok(_) => Bool.False
		Err(Missing) => Bool.True
	}

shape_is : Shape, Str -> Bool
shape_is = |shape, expected_foo|
	if shape.foo_bar == expected_foo {
		is_missing(shape.maybe_token)
	} else {
		Bool.False
	}
