JsonParseInferredFieldTypes :: [].{}

# Nothing in this file has a type annotation: the decoded record's field types
# are inferred from how each field is used after `Json.parse`.
Email := [Email(Str)].{
	parser_for = |encoding| {
		parse_str = Str.parser_for(encoding)
		|state| {
			parsed = parse_str(state)?
			if Str.contains(parsed.value, "@") {
				Ok({ value: Email.Email(parsed.value), rest: parsed.rest })
			} else {
				Err(InvalidEmail(parsed.value))
			}
		}
	}

	to_str = |Email.Email(address)| address
}

summarize = |json| {
	{ name, email, num_stars } = Json.parse(json)?
	Ok(Str.concat(name, " <${Email.to_str(email)}> ${Str.repeat("*", num_stars)}"))
}

expect summarize("{\"name\":\"Ann\",\"email\":\"ann@example.com\",\"num_stars\":3}") == Ok("Ann <ann@example.com> ***")

expect {
	match summarize("{\"name\":\"Ann\",\"email\":\"not an email\",\"num_stars\":3}") {
		Err(InvalidEmail(raw)) => raw == "not an email"
		_ => False
	}
}

expect {
	match summarize("{\"name\":\"Ann\",\"email\":\"ann@example.com\",\"num_stars\":-3}") {
		Err(InvalidJson(_)) => True
		_ => False
	}
}

expect {
	match summarize("{\"name\":\"Ann\",\"num_stars\":3}") {
		Err(MissingRequiredField(field)) => field == "email"
		_ => False
	}
}
