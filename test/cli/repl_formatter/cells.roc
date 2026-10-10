module [decode, encode]

decode : Str -> Try(Str, Str)
decode = |line| match Json.parse(line) {
	Ok(source) => Ok(source)
	Err(_) => Err("invalid input")
}

encode = |response| Json.to_str({
	result: response.result,
	stdout: response.stdout,
	diagnostics: response.diagnostics,
})
