import Base exposing [ArgParserResult, parse_args]
import Builder exposing [CliBuilder]

Cli := [].{
	CliParser(state) : {
		parser : List(Str) -> ArgParserResult(state),
	}

	map : CliBuilder(a, from_action, to_action), (a -> b) -> CliBuilder(b, from_action, to_action)
	map = |builder, mapper| Builder.map(builder, mapper)

	map2 : CliBuilder(a, action1, action2), CliBuilder(b, action2, action3), (a, b -> c) -> CliBuilder(c, action1, action3)
	map2 = |left, right, combiner| Builder.combine(left, right, combiner)

	finish : CliBuilder(data, from_action, to_action) -> CliParser(data)
	finish = |builder| {
		{ parser } = Builder.into_parts(builder)

		{
			parser: |args|
				match parser({ args: parse_args(args) }) {
					IncorrectUsage(arg_extract_err) => ArgParserResult.IncorrectUsage(arg_extract_err)
					SuccessfullyParsed({ data, .. }) => ArgParserResult.SuccessfullyParsed(data)
				},
		}
	}

	}
