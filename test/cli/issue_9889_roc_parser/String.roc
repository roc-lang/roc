import Parser

String :: {}.{

	## ```
	## Utf8 : List U8
	## ```
	Utf8 : List(U8)

	## Parse a `Str` using a [Parser]
	## ```roc
	## color : Parser(Utf8, [Red, Green, Blue])
	## color = {
	##     one_of(
	##         [
	##             Parser.const(Red).skip(string("red")),
	##             Parser.const(Green).skip(string("green")),
	##             Parser.const(Blue).skip(string("blue")),
	##         ]
	##     )
	## }
	##
	## expect parse_str(color, "green") == Ok(Green)
	## ```
	parse_str : Parser(Utf8, a), Str -> Try(a, [ParsingFailure(Str), ParsingIncomplete(Str)])
	parse_str = |parser, input| {
		parser
			->parse_utf8(str_to_raw(input))
			.map_err(
				|problem| {
					match problem {
						ParsingFailure(msg) => ParsingFailure(msg)
						ParsingIncomplete(leftover_raw) => ParsingIncomplete(str_from_utf8(leftover_raw))
					}
				},
			)
	}

	## Runs a parser against the start of a string, allowing the parser to consume it only partially.
	##
	## - If the parser succeeds, returns the resulting value as well as the leftover input.
	## - If the parser fails, returns `Err (ParsingFailure msg)`
	##
	## ```roc
	## at_sign : Parser(Utf8, [AtSign])
	## at_sign = Parser.const(AtSign).skip(codeunit('@'))
	##
	## expect parse_str(at_sign, "@") == Ok(AtSign)
	## expect at_sign->parse_str_partial("@").map_ok(|r| { r.val }) == Ok(AtSign)
	## expect at_sign->parse_str_partial("$").is_err()
	## ```
	parse_str_partial : Parser(Utf8, a), Str -> Try({ val : a, input : Str }, [ParsingFailure(Str)])
	parse_str_partial = |parser, input| {
		parser
			->parse_utf8_partial(str_to_raw(input))
			.map_ok(
				|{ val: val, input: rest_raw }| {
					{ val: val, input: str_from_utf8(rest_raw) }
				},
			)
	}

	## Runs a parser against a string, requiring the parser to consume it fully.
	##
	## - If the parser succeeds, returns `Ok a`
	## - If the parser fails, returns `Err (ParsingFailure Str)`
	## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete (List U8))`
	##
	parse_utf8 : Parser(Utf8, a), Utf8 -> Try(a, [ParsingFailure(Str), ParsingIncomplete(Utf8)])
	parse_utf8 = |parser, input| {
		parser.parse(
			input,
			|leftover| {
				leftover.len() == 0
			},
		)
	}

	## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
	parse_utf8_partial : Parser(Utf8, a), Utf8 -> Try({ val : a, input : Utf8 }, [ParsingFailure(Str)])
	parse_utf8_partial = |parser, input| {
		parser.parse_partial(input)
	}

	## ```roc
	## is_digit : U8 -> Bool
	## is_digit = |b| { b >= '0' and b <= '9' }
	##
	## expect codeunit_satisfies->parse_str(is_digit, "0") == Ok('0')
	## expect codeunit_satisfies->parse_str(is_digit, "*").is_err()
	## ```
	codeunit_satisfies : (U8 -> Bool) -> Parser(Utf8, U8)
	codeunit_satisfies = |check| {
		Parser.build_primitive_parser(
			|input| {
				{ before: start, others: input_rest } = input.split_at(1)

				match start.get(0) {
					Err(OutOfBounds) =>
						Err(ParsingFailure("expected a codeunit satisfying a condition, but input was empty."))

					Ok(start_codeunit) => {
						if check(start_codeunit) {
							Ok({ val: start_codeunit, input: input_rest })
						} else {
							other_char = str_from_codeunit(start_codeunit)
							input_str = str_from_utf8(input)

							Err(ParsingFailure("expected a codeunit satisfying a condition but found `${other_char}`.\n While reading: `${input_str}`"))
						}
					}
				}
			},
		)
	}

	## ```roc
	## at_sign : Parser(Utf8, [AtSign])
	## at_sign = Parser.const(AtSign).skip(codeunit('@'))
	##
	## expect at_sign->parse_str("@") == Ok(AtSign)
	## expect at_sign->parse_str_partial("$").is_err()
	## ```
	codeunit : U8 -> Parser(Utf8, U8)
	codeunit = |expected_code_unit| {
		Parser.build_primitive_parser(
			|input| {
				match input {
					[] =>
						Err(ParsingFailure("expected char `${str_from_codeunit(expected_code_unit)}` but input was empty."))

					[first, .. as rest] if first == expected_code_unit =>
						Ok({ val: expected_code_unit, input: rest })

					[first, ..] =>
						Err(ParsingFailure("expected char `${str_from_codeunit(expected_code_unit)}` but found `${str_from_codeunit(first)}`.\n While reading: `${str_from_utf8(input)}`"))
					}
			},
		)
	}

	## Parse an exact sequence of utf8
	utf8 : List(U8) -> Parser(Utf8, List(U8))
	utf8 = |expected_string| {
		# Implemented manually instead of a sequence of codeunits
		# because of efficiency and better error messages
		Parser.build_primitive_parser(
			|input| {
				{ before: start, others: input_rest } = input.split_at(expected_string.len())

				if start == expected_string {
					Ok({ val: expected_string, input: input_rest })
				} else {
					error_string = str_from_utf8(expected_string)
					other_string = str_from_utf8(start)
					input_string = str_from_utf8(input)

					Err(ParsingFailure("expected string `${error_string}` but found `${other_string}`.\nWhile reading: ${input_string}"))
				}
			},
		)
	}

	## Parse the given `Str`
	## ```roc
	## expect string("Foo")->parse_str("Foo") == Ok("Foo")
	## expect string("Foo")->parse_str("Bar").is_err()
	## ```
	string : Str -> Parser(Utf8, Str)
	string = |expected_string| {
		expected_string
			->str_to_raw()
			->utf8()
			.map(
				|_val| {
					expected_string
				},
			)
	}

	## Matches any `U8` codeunit
	## ```roc
	## expect parse_str(any_codeunit, "a") == Ok('a')
	## expect parse_str(any_codeunit, "$") == Ok('$')
	## ```
	any_codeunit : Parser(Utf8, U8)
	any_codeunit = codeunit_satisfies(
		|_| {
			Bool.True
		},
	)
	expect any_codeunit->parse_str("a") == Ok('a')
	expect any_codeunit->parse_str("\$") == Ok(36)

	## Matches any `Utf8` and consumes all the input without fail.
	## ```roc
	## expect {
	##     bytes = "consumes all the input".to_utf8()
	##     any_thing.parse(bytes, List.is_empty) == Ok(bytes)
	## }
	## ```
	any_thing : Parser(Utf8, Utf8)
	any_thing = Parser.build_primitive_parser(
		|input| {
			Ok({ val: input, input: [] })
		},
	)
	expect {
		bytes = "consumes all the input".to_utf8()
		any_thing.parse(bytes, List.is_empty) == Ok(bytes)
	}

	# Matches any string
	# as long as it is valid UTF8.
	any_string : Parser(Utf8, Str)
	any_string = Parser.build_primitive_parser(
		|field_utf8ing| {
			match Str.from_utf8(field_utf8ing) {
				Ok(string_val) =>
					Ok({ val: string_val, input: [] })

				Err(BadUtf8(_)) =>
					Err(ParsingFailure("Expected a string field, but its contents cannot be parsed as UTF8."))
				}
		},
	)

	## ```roc
	## expect digit->parse_str("0") == Ok(0)
	## expect digit->parse_str("not a digit").is_err()
	## ```
	digit : Parser(Utf8, U64)
	digit = 
		Parser.build_primitive_parser(
			|input| {
				match input {
					[] =>
						Err(ParsingFailure("Expected a digit from 0-9 but input was empty."))

					[first, .. as rest] if first >= '0' and first <= '9' =>
						Ok({ val: (first - '0').to_u64(), input: rest })

					_ =>
						Err(ParsingFailure("Not a digit"))
					}
			},
		)

	## Parse a sequence of digits into a `U64`, accepting leading zeroes
	## ```roc
	## expect digits->parse_str("0123") == Ok(123)
	## expect digits->parse_str("not a digit").is_err()
	## ```
	digits : Parser(Utf8, U64)
	digits = 
		Parser.one_or_more(digit)
			.map(
				|ds| {
					ds.fold(
						0,
						|sum, d| {
							sum * 10 + d
						},
					)
				},
			)

	## Try a bunch of different parsers.
	##
	## The first parser which is tried is the one at the front of the list,
	## and the next one is tried until one succeeds or the end of the list was reached.
	## ```roc
	## bool_parser : Parser(Utf8, Bool)
	## bool_parser = {
	##     one_of([string("true"), string("false")])
	##     .map(|x| { x == "true" })
	## }
	##
	## expect bool_parser->parse_str("true") == Ok(Bool.True)
	## expect bool_parser->parse_str("false") == Ok(Bool.False)
	## expect bool_parser->parse_str("not a bool").is_err()
	## ```
	one_of : List(Parser(Utf8, a)) -> Parser(Utf8, a)
	one_of = |parsers| {
		Parser.build_primitive_parser(
			|input| {
				parsers.fold_until(
					Err(ParsingFailure("(no possibilities)")),
					|_, parser| {
						match parse_utf8_partial(parser, input) {
							Ok(val) =>
								Break(Ok(val))

							Err(problem) =>
								Continue(Err(problem))
							}
					},
				)
			},
		)
	}

	str_from_utf8 : Utf8 -> Str
	str_from_utf8 = |raw_str| {
		raw_str
			->Str.from_utf8()
			?? {
				crash "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"
			}
	}

	str_from_ascii : U8 -> Str
	str_from_ascii = |ascii_num| {
		match Str.from_utf8([ascii_num]) {
			Ok(answer) => answer
			Err(_) => {
				crash "The number ${ascii_num.to_str()} is not a valid ASCII constant!"
			}
		}
	}
}

str_to_raw : Str -> String.Utf8
str_to_raw = |str| {
	str.to_utf8()
}

str_from_codeunit : U8 -> Str
str_from_codeunit = |cu| {
	String.str_from_utf8([cu])
}

expect String.parse_str(String.any_codeunit, "a") == Ok('a')
expect String.parse_str(String.any_codeunit, "\$") == Ok(36)

expect {
	bytes = "consumes all the input".to_utf8()
	Parser.parse(
		String.any_thing,
		bytes,
		|l| {
			l.is_empty()
		},
	) == Ok(bytes)
}

# -------------------- example snippets used in docs --------------------

parse_u32 : Parser(String.Utf8, U32)
parse_u32 = 
	Parser.const(U64.to_u32_wrap).keep(String.digits)

expect String.parse_str(parse_u32, "123") == Ok(123.U32)

color : Parser(String.Utf8, [Red, Green, Blue])
color = 
	String.one_of(
		[
			Parser.const(Red).skip(String.string("red")),
			Parser.const(Green).skip(String.string("green")),
			Parser.const(Blue).skip(String.string("blue")),
		],
	)

expect String.parse_str(color, "green") == Ok(Green)

parse_numbers : Parser(String.Utf8, List(U64))
parse_numbers = (String.digits).sep_by(String.codeunit(','))

expect String.parse_str(parse_numbers, "1,2,3") == Ok([1, 2, 3])

expect String.parse_str(String.string("Foo"), "Foo") == Ok("Foo")
expect String.parse_str(String.string("Foo"), "Bar").is_err()

ignore_text : Parser(String.Utf8, U64)
ignore_text = 
	Parser.const(
		|d| {
			d
		},
	)
		.skip(Parser.chomp_until(':'))
		.skip(String.codeunit(':'))
		.keep(String.digits)

expect String.parse_str(ignore_text, "ignore preceding text:123") == Ok(123)

ignore_numbers : Parser(String.Utf8, Str)
ignore_numbers = 
	Parser.const(
		|str| {
			str
		},
	)
		.skip(
			Parser.chomp_while(
				|b| {
					b >= '0' and b <= '9'
				},
			),
		)
		.keep(String.string("TEXT"))

expect String.parse_str(ignore_numbers, "0123456789876543210TEXT") == Ok("TEXT")

is_digit : U8 -> Bool
is_digit = |b| {
	b >= '0' and b <= '9'
}

expect String.parse_str(String.codeunit_satisfies(is_digit), "0") == Ok('0')
expect String.parse_str(String.codeunit_satisfies(is_digit), "*").is_err()

at_sign : Parser(String.Utf8, [AtSign])
at_sign = Parser.const(AtSign).skip(String.codeunit('@'))

expect String.parse_str(at_sign, "@") == Ok(AtSign)
expect String.parse_str_partial(at_sign, "@").map_ok(
	|r| {
		r.val
	},
) == Ok(AtSign)
expect String.parse_str_partial(at_sign, "\$").is_err()

Requirement : [Green(U64), Red(U64), Blue(U64)]

RequirementSet : List(Requirement)

Game : { id : U64, requirements : List(RequirementSet) }

parse_game : Str -> Try(Game, [ParsingError])
parse_game = |s| {
	green = Parser.const(|x| Green(x)).keep(String.digits).skip(String.string(" green"))
	red = Parser.const(|x| Red(x)).keep(String.digits).skip(String.string(" red"))
	blue = Parser.const(|x| Blue(x)).keep(String.digits).skip(String.string(" blue"))

	requirement_set : Parser(_, RequirementSet)
	requirement_set = String.one_of([green, red, blue]).sep_by(String.string(", "))

	requirements : Parser(_, List(RequirementSet))
	requirements = requirement_set.sep_by(String.string("; "))

	game : Parser(_, Game)
	game = 
		Parser.const(
			|id| {
				|r| {
					{ id, requirements: r }
				}
			},
		)
			.skip(String.string("Game "))
			.keep(String.digits)
			.skip(String.string(": "))
			.keep(requirements)

	match String.parse_str(game, s) {
		Ok(g) => Ok(g)
		Err(ParsingFailure(_)) | Err(ParsingIncomplete(_)) => Err(ParsingError)
	}
}

expect {
	parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
		== Ok(
			{
				id: 1,
				requirements: [
					[Blue(3), Red(4)],
					[Red(1), Green(2), Blue(6)],
					[Green(2)],
				],
			},
		)
}

expect String.parse_str(String.digit, "0") == Ok(0)
expect String.parse_str(String.digit, "not a digit").is_err()

expect String.parse_str(String.digits, "0123") == Ok(123)
expect String.parse_str(String.digits, "not a digit").is_err()

bool_parser : Parser(String.Utf8, Bool)
bool_parser = 
	String.one_of([String.string("true"), String.string("false")])
		.map(
			|x| {
				x == "true"
			},
		)

expect String.parse_str(bool_parser, "true") == Ok(Bool.True)
expect String.parse_str(bool_parser, "false") == Ok(Bool.False)
expect String.parse_str(bool_parser, "not a bool").is_err()
