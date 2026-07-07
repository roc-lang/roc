import Base exposing [ArgParserResult, parse_args]
import Builder exposing [CliBuilder]

Cli := [].{
	CliParser(state) : {
		parser : List(Str) -> ArgParserResult(state),
	}

	finish : CliBuilder(data) -> CliParser(data)
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
