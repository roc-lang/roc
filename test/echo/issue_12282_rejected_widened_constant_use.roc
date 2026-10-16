# Parsing into `{ host : Str }` can also fail with `MissingRequiredField`,
# which the annotation's error union does not list. The derived parser adds
# that tag once checking settles, after the use in `main!` copied `config`'s
# type, and the `match` there does not admit it, so the use is rejected too.
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
