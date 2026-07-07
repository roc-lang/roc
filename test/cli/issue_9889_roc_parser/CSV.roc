import Parser
import String

# # This is a CSV parser which follows RFC4180
# #
# # For simplicity's sake, the following things are not yet supported:
# # - CSV files with headings
# #
# # The following however *is* supported
# # - A simple LF ("\n") instead of CRLF ("\r\n") to separate records.
CSV :: { records : List(List(String.Utf8)) }.{
	CSVRecord : List(CSVField)
	CSVField : String.Utf8

	# # Attempts to Parser.parse an `a` from a `Str` that is encoded in CSV format.
	parse_str : Parser(CSVRecord, a), Str -> Try(List(a), [ParsingFailure(Str), SyntaxError(Str), ParsingIncomplete(CSVRecord)])
	parse_str = |csv_parser, input| {
		match parse_str_to_csv(input) {
			Err(ParsingIncomplete(rest)) => {
				rest_str = String.str_from_utf8(rest)

				Err(SyntaxError(rest_str))
			}

			Err(ParsingFailure(str)) => {
				Err(ParsingFailure(str))
			}

			Ok(csv_data) => {
				match parse_csv(csv_parser, csv_data) {
					Err(ParsingFailure(str)) => {
						Err(ParsingFailure(str))
					}

					Err(ParsingIncomplete(problem)) => {
						Err(ParsingIncomplete(problem))
					}

					Ok(vals) => {
						Ok(vals)
					}
				}
			}
		}
	}

	# # Attempts to Parser.parse an `a` from a `CSV` datastructure (a list of lists of bytestring-fields).
	parse_csv : Parser(CSVRecord, a), CSV -> Try(List(a), [ParsingFailure(Str), ParsingIncomplete(CSVRecord)])
	parse_csv = |csv_parser, { records: csv_data }| {
		csv_data
			.map_with_index(
				|record_fields_list, index| {
					{ record: record_fields_list, index: index }
				},
			)
			.fold_until(
				Try.Ok([]),
				|state, { record: record_fields_list, index: index }| {
					match parse_csv_record(csv_parser, record_fields_list) {
						Err(ParsingFailure(problem)) => {
							index_str = (index + 1).to_str()
							record_str = 
								record_fields_list
									.map(String.str_from_utf8)
									.map(
										|val| {
											"\"${val}\""
										},
									)
									->Str.join_with(", ")
							problem_str = "${problem}\nWhile parsing record no. ${index_str}: `${record_str}`"

							Break(Err(ParsingFailure(problem_str)))
						}

						Err(ParsingIncomplete(problem)) => {
							Break(Err(ParsingIncomplete(problem)))
						}

						Ok(val) => {
							state
								.map_ok(
									|vals| {
										vals.append(val)
									},
								)
								->Continue()
						}
					}
				},
			)
	}

	# # Attempts to Parser.parse an `a` from a `CSVRecord` datastructure (a list of bytestring-fields)
	##
	# # This parser succeeds when all fields of the CSVRecord are consumed by the parser.
	parse_csv_record : Parser(CSVRecord, a), CSVRecord -> Try(a, [ParsingFailure(Str), ParsingIncomplete(CSVRecord)])
	parse_csv_record = |csv_parser, record_fields_list| {
		Parser.parse(
			csv_parser,
			record_fields_list,
			|leftover| {
				leftover == []
			},
		)
	}

	# # Wrapper function to combine a set of fields into your desired `a`
	##
	# # ```roc
	# # record(|first_name| |last_name| |age| User({ first_name, last_name, age }))
	# # .field(string)
	# # .field(string)
	# # .field(u64)
	# # ```
	record : a -> Parser(CSVRecord, a)
	record = |f| {
		Parser.const(f)
	}

	# # Turns a parser for a `List(U8)` into a parser that parses part of a `CSVRecord`.
	field : Parser(String.Utf8, a) -> Parser(CSVRecord, a)
	field = |field_parser| {
		Parser.build_primitive_parser(
			|fields_list| {
				match fields_list.get(0) {
					Err(OutOfBounds) =>
						Err(ParsingFailure("expected another CSV field but there are no more fields in this record"))

					Ok(raw_str) => {
						match String.parse_utf8(field_parser, raw_str) {
							Ok(val) => {
								Ok({ val: val, input: fields_list.drop_first(1) })
							}

							Err(ParsingFailure(reason)) => {
								field_str = raw_str->String.str_from_utf8()

								Err(ParsingFailure("Field `${field_str}` could not be parsed. ${reason}"))
							}

							Err(ParsingIncomplete(reason)) => {
								reason_str = String.str_from_utf8(reason)
								fields_str = 
									fields_list
										.map(String.str_from_utf8)
										->Str.join_with(", ")

								Err(ParsingFailure("The field parser was unable to read the whole field: `${reason_str}` while parsing the first field of leftover ${fields_str})"))
							}
						}
					}
				}
			},
		)
	}

	# # Parser for a field containing a UTF8-encoded string
	string : Parser(CSVField, Str)
	string = String.any_string

	# # Parse a number from a CSV field
	u64 : Parser(CSVField, U64)
	u64 = 
		string
			.map(
				|val| {
					match U64.from_str(val) {
						Ok(num) => Ok(num)
						Err(_) => Err("${val} is not a U64.")
					}
				},
			)
			.flatten()

	# # Parse a 64-bit float from a CSV field
	f64 : Parser(CSVField, F64)
	f64 = 
		string
			.map(
				|val| {
					match F64.from_str(val) {
						Ok(num) => Ok(num)
						Err(_) => Err("${val} is not a F64.")
					}
				},
			)
			.flatten()

	# # Attempts to Parser.parse a Str into the internal `CSV` datastructure (A list of lists of bytestring-fields).
	parse_str_to_csv : Str -> Try(CSV, [ParsingFailure(Str), ParsingIncomplete(String.Utf8)])
	parse_str_to_csv = |input| {
		Parser.parse(
			file,
			input.to_utf8(),
			|leftover| {
				leftover == []
			},
		)
	}

	# # Attempts to Parser.parse a Str into the internal `CSVRecord` datastructure (A list of bytestring-fields).
	parse_str_to_csv_record : Str -> Try(CSVRecord, [ParsingFailure(Str), ParsingIncomplete(String.Utf8)])
	parse_str_to_csv_record = |input| {
		Parser.parse(
			csv_record,
			input.to_utf8(),
			|leftover| {
				leftover == []
			},
		)
	}

	# The following are parsers to turn strings into CSV structures
	file : Parser(String.Utf8, CSV)
	file = 
		Parser.sep_by(csv_record, end_of_line)
			.map(
				|records| {
					{ records }
				},
			)
}

csv_record : Parser(String.Utf8, CSV.CSVRecord)
csv_record = Parser.sep_by1(csv_field, comma)

csv_field : Parser(String.Utf8, CSV.CSVField)
csv_field = Parser.alt(escaped_csv_field, nonescaped_csv_field)

escaped_csv_field : Parser(String.Utf8, CSV.CSVField)
escaped_csv_field = Parser.between(escaped_contents, dquote, dquote)

escaped_contents : Parser(String.Utf8, List(U8))
escaped_contents = 
	String.one_of(
		[
			twodquotes.map(
				|_| {
					'"'
				},
			),
			comma,
			cr,
			lf,
			textdata,
		],
	)
		.many()

twodquotes : Parser(String.Utf8, Str)
twodquotes = String.string("\"\"")

nonescaped_csv_field : Parser(String.Utf8, CSV.CSVField)
nonescaped_csv_field = textdata.many()

comma = String.codeunit(',')

dquote = String.codeunit('"')

end_of_line = Parser.alt(Parser.ignore(crlf), Parser.ignore(lf))

cr = String.codeunit('\r')

lf = String.codeunit('\n')

crlf = String.string("\r\n")

textdata = String.codeunit_satisfies(
	|x| {
		(x >= 32 and x <= 33) or (x >= 35 and x <= 43) or (x >= 45 and x <= 126)
	},
)
