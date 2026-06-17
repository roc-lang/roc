ParseFromOptionalOnlyRecordNoMissingMethod :: [].{}

Format := [Present(Str)].{
	parse_str : Format -> Try({ value : Str, rest : Format }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)

	parse_record_field : U64, Format -> Try(
		[
			Field({ name : Str, value : Format, rest : Format }),
			Continue({ rest : Format }),
			Done({ rest : Format }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, slot| Ok(Done({ rest: slot }))
}

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parse_from : Format -> Try({ value : a, rest : Format }, [MissingRequired]),
	]
parse = |input| {
	Shape : a
	parsed = Shape.parse_from(Format.Present(input))?
	Ok(parsed.value)
}

main : Try({ foo : Try(Str, [Missing]) }, [MissingRequired])
main = parse("")
