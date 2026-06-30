ParserBoxedRecursiveConst :: [].{}

Parser(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{
	parse : Parser(input, val), input -> [Ok(val, input), Err(Str)]
	parse = |parser, input|
		(parser.run)(input)

	fail : Str -> Parser(input, val)
	fail = |message|
		{ run: |_| Err(message) }

	succeed : val -> Parser(input, val)
	succeed = |value|
		{ run: |input| Ok(value, input) }

	map2 : Parser(input, a), Parser(input, b), (a, b -> c) -> Parser(input, c)
	map2 = |first, second, combine|
		{
			run: |input|
				match Parser.parse(first, input) {
					Err(message) => Err(message)
					Ok(first_value, rest) =>
						match Parser.parse(second, rest) {
							Err(message) => Err(message)
							Ok(second_value, rest2) => Ok(combine(first_value, second_value), rest2)
						}
					},

		}

	alt : Parser(input, val), Parser(input, val) -> Parser(input, val)
	alt = |first, second|
		{
			run: |input|
				match Parser.parse(first, input) {
					Ok(value, rest) => Ok(value, rest)
					Err(_) => Parser.parse(second, input)
				},

		}

	lazy : ({} -> Parser(input, val)) -> Parser(input, val)
	lazy = |thunk|
		{ run: |input| Parser.parse(thunk({}), input) }
}

step : Parser(U64, U64)
step = {
	run: |input|
		if input > 0 {
			Ok(input, input - 1)
		} else {
			Err("done")
		},
}

recursive : Parser(U64, List(U64))
recursive =
	Parser.alt(
		Parser.map2(step, Parser.lazy(|_| recursive), |head, tail| List.prepend(tail, head)),
		Parser.succeed([]),
	)

expect Parser.parse(recursive, 3) == Ok([3, 2, 1], 0)
