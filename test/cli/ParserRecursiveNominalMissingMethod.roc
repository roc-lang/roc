ParserRecursiveNominalMissingMethod :: [].{}

Tree := [Leaf(Str), Node(Tree)].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

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
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "tree", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : Format, State -> Try(State, [FormatError, ..])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

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

main : Try({ tree : Tree }, [FormatError, MissingRequiredField(Str)])
main = parse(State.Present("leaf"))
