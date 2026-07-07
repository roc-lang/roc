import Base exposing [
	ArgParser,
	ArgParserResult,
	ParsedArg,
]

Builder(data) := {
	parser : ArgParser(data),
}.{
	CliBuilder(data) : Builder(data)

	from_arg_parser : (List(ParsedArg) -> Try({ data : data, remaining_args : List(ParsedArg) }, [ArgErr])) -> CliBuilder(data)
	from_arg_parser = |parse_args_fn| {
		new_parser = |{ args }|
			match parse_args_fn(args) {
				Ok({ data, remaining_args }) =>
					ArgParserResult.SuccessfullyParsed({ data, remaining_args })

				Err(err) =>
					ArgParserResult.IncorrectUsage(err)
			}

		{ parser: new_parser }
	}

	into_parts : CliBuilder(state) -> { parser : ArgParser(state) }
	into_parts = |{ parser: state_parser }| { parser: state_parser }

}
