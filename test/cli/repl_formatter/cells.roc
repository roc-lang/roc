module [decode, encode]

decode : Str -> [Evaluate(Str), Reply(Str)]
decode = |line| match Json.parse(line) {
	Ok(source) => Evaluate(source)
	Err(_) => Reply("invalid input")
}

encode = |_input, response| Json.to_str({
	status: Str.inspect(response.status),
	result: response.result,
	stdout: response.stdout,
	diagnostics: response.diagnostics,
})
