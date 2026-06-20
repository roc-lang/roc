ParserRecursiveNominalMissingMethod :: [].{}

Tree := [Leaf(Str), Node(Tree)].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

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
	parse_record_field = |_, state|
		match state {
			Present(_) => Ok(TryField({ name: "tree", rest: state }))
			Done => Ok(Done({ rest: state }))
		}

	skip_record_field : State -> Try(State, [MissingRequired])
	skip_record_field = |_| Ok(Done)

	missing_record_field : Str, State -> [MissingRequired]
	missing_record_field = |_, _| MissingRequired
}

State := [Present(Str), Done]

parse : State -> Try(a, [MissingRequired])
	where [
		a.parser_for : Format -> (State -> Try({ value : a, rest : State }, [MissingRequired])),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	Ok(parsed.value)
}

main : Try({ tree : Tree }, [MissingRequired])
main = parse(State.Present("leaf"))
