Base := [].{
	ParsedArg : [
		Short,
		ShortGroup,
		Long,
		Parameter(Str),
	]

	parse_args : List(Str) -> List(ParsedArg)
	parse_args = |args|
		args
			.drop_first(1)
			.map(|arg| Parameter(arg))

	ArgParserResult(a) := [
		IncorrectUsage([ArgErr]),
		SuccessfullyParsed(a),
	]

	ArgParserState(a) : { data : a, remaining_args : List(ParsedArg) }

	ArgParser(a) : { args : List(ParsedArg) } -> ArgParserResult(ArgParserState(a))

}
