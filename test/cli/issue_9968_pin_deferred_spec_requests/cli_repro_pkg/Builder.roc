import Base exposing [
	ArgParser,
	ArgParserResult,
	ParsedArg,
]

Builder(data, from_action, to_action) := {
	parser : ArgParser(data),
}.{
	CliBuilder(data, from_action, to_action) : Builder(data, from_action, to_action)

	from_arg_parser : (List(ParsedArg) -> Try({ data : data, remaining_args : List(ParsedArg) }, [ArgErr])) -> CliBuilder(data, from_action, to_action)
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

	into_parts : CliBuilder(state, from_action, to_action) -> { parser : ArgParser(state) }
	into_parts = |{ parser: state_parser }| { parser: state_parser }

	map : CliBuilder(a, from_action, to_action), (a -> b) -> CliBuilder(b, from_action, to_action)
	map = |{ parser: state_parser }, mapper| {
		combined_parser = |input|
			match state_parser(input) {
				IncorrectUsage(arg_extract_err) => ArgParserResult.IncorrectUsage(arg_extract_err)
				SuccessfullyParsed({ data, remaining_args }) =>
					ArgParserResult.SuccessfullyParsed({ data: mapper(data), remaining_args })
			}

		{ parser: combined_parser }
	}

	combine : CliBuilder(a, action1, action2), CliBuilder(b, action2, action3), (a, b -> c) -> CliBuilder(c, action1, action3)
	combine = |{ parser: left_parser }, { parser: right_parser }, combiner| {
		combined_parser = |input|
			match left_parser(input) {
				IncorrectUsage(arg_extract_err) => ArgParserResult.IncorrectUsage(arg_extract_err)
				SuccessfullyParsed({ data, remaining_args }) =>
					match right_parser({ args: remaining_args }) {
						IncorrectUsage(arg_extract_err) => ArgParserResult.IncorrectUsage(arg_extract_err)
						SuccessfullyParsed({ data: data2, remaining_args: rest_of_args }) =>
							ArgParserResult.SuccessfullyParsed({ data: combiner(data, data2), remaining_args: rest_of_args })
					}
			}

		{ parser: combined_parser }
	}

}
