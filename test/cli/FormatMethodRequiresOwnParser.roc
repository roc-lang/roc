FormatMethodRequiresOwnParser :: [].{}

Rec : { name : Str }

# A format whose string method asks, through its own `where` clause, for the
# parser of its state. The state is the record being parsed, so the record's
# generated parser calls a method that calls that same parser.
Fmt := [Default].{
	rename_field : Fmt, Str -> Str
	rename_field = |_, name| name

	parse_record_start : Fmt, s -> Try([Counted({ len : U64, rest : s }), Uncounted(s)], [Bad])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Fmt,
	Encoding.FieldName.FieldNames(_shape),
	s -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : s }),
			TryField({ name : Str, rest : s }),
			TryFieldCaseless({ name : Str, rest : s }),
			Continue(s),
			Done(s),
		],
		[Bad],
	)
	parse_record_field = |_, _, state| Ok(Done(state))

	parse_record_after_field : Fmt, s -> Try([Continue(s), Done(s)], [Bad])
	parse_record_after_field = |_, state| Ok(Done(state))

	skip_record_field : Fmt, s -> Try(s, [Bad])
	skip_record_field = |_, state| Ok(state)

	parse_str : Fmt, s -> Try({ value : Str, rest : s }, [Bad, MissingRequiredField(Str)])
		where [s.parser_for : Fmt -> (s -> Try({ value : s, rest : s }, [Bad, MissingRequiredField(Str)]))]
	parse_str = |fmt, state| {
		Shape : s
		parsed = Shape.parser_for(fmt)(state)?
		Ok({ value: "", rest: parsed.rest })
	}

	invalid_value : Fmt, s -> [Bad]
	invalid_value = |_, _| Bad
}

# The same format declared in a function body, whose string method names the
# body's type variable, so the method is a local procedure of the body.
make_parser : a -> (Rec -> Try({ value : Rec, rest : Rec }, [Bad, MissingRequiredField(Str)]))
make_parser = |_x| {
	LocalFmt := [Default].{
		rename_field : LocalFmt, Str -> Str
		rename_field = |_, name| name

		parse_record_start : LocalFmt, s -> Try([Counted({ len : U64, rest : s }), Uncounted(s)], [Bad])
		parse_record_start = |_, state| Ok(Uncounted(state))

		parse_record_field : LocalFmt,
		Encoding.FieldName.FieldNames(_shape),
		s -> Try(
			[
				Field({ field : Encoding.FieldName(_shape), rest : s }),
				TryField({ name : Str, rest : s }),
				TryFieldCaseless({ name : Str, rest : s }),
				Continue(s),
				Done(s),
			],
			[Bad],
		)
		parse_record_field = |_, _, state| Ok(Done(state))

		parse_record_after_field : LocalFmt, s -> Try([Continue(s), Done(s)], [Bad])
		parse_record_after_field = |_, state| Ok(Done(state))

		skip_record_field : LocalFmt, s -> Try(s, [Bad])
		skip_record_field = |_, state| Ok(state)

		parse_str : LocalFmt, s -> Try({ value : Str, rest : s }, [Bad, MissingRequiredField(Str)])
			where [s.parser_for : LocalFmt -> (s -> Try({ value : s, rest : s }, [Bad, MissingRequiredField(Str)]))]
		parse_str = |fmt, state| {
			empty : List(a)
			empty = []
			_unused = List.len(empty)
			Shape : s
			parsed = Shape.parser_for(fmt)(state)?
			Ok({ value: "", rest: parsed.rest })
		}

		invalid_value : LocalFmt, s -> [Bad]
		invalid_value = |_, _| Bad
	}
	parser_of : value -> (Rec -> Try({ value : value, rest : Rec }, [Bad, MissingRequiredField(Str)]))
		where [value.parser_for : LocalFmt -> (Rec -> Try({ value : value, rest : Rec }, [Bad, MissingRequiredField(Str)]))]
	parser_of = |_sample| {
		Shape : value
		|st| Shape.parser_for(LocalFmt.Default)(st)
	}
	sample_name : Str
	sample_name = ""
	parser_of({ name: sample_name })
}

parse_rec : Rec -> Try({ value : Rec, rest : Rec }, [Bad, MissingRequiredField(Str)])
parse_rec = |state| {
	Shape : Rec
	Shape.parser_for(Fmt.Default)(state)
}

# Every record field is reported missing, as this format lists none.
expect parse_rec({ name: "x" }) == Err(MissingRequiredField("name"))

expect make_parser(5.U64)({ name: "x" }) == Err(MissingRequiredField("name"))

expect make_parser("y")({ name: "x" }) == Err(MissingRequiredField("name"))

