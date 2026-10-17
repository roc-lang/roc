# Parsing into `{ host : Str }` can also fail with `MissingRequiredField`,
# which the annotation's error union does not list. The annotation closes that
# union, so the derived parser's demand for the tag is rejected once, at the
# parse call, and the use in `main!` is not reported again.
config : Try({ host : Str }, [InvalidJson(Str)])
config = Json.parse("{}")

main! = |_args| {
	echo!("before")
	host = match config {
		Ok(parsed) => parsed.host
		Err(InvalidJson(message)) => message
	}
	echo!(host)
	Ok({})
}
