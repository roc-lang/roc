module [decode, encode]

Request : [Evaluate(Str), Reply(Str)]

Input : { id : Str, code : Str, show_result : Bool }

Response : {
	status : [Success, Incomplete, Failure],
	result : Str,
	stdout : Str,
	diagnostics : Str,
	value : [Other, Tag({ name : Str, payload : Try(Str, {}) })],
}

decode : Str -> Request
decode = |line| {
	if line == "ping" {
		Reply("pong")
	} else {
		request : Try(Input, [InvalidJson(Str), MissingRequiredField(Str)])
		request = Json.parse(line)
		match request {
			Ok({ code, .. }) => Evaluate(code)
			Err(_) => Reply("bad request")
		}
	}
}

encode : Str, Response -> Str
encode = |input, response| {
	request : Try(Input, [InvalidJson(Str), MissingRequiredField(Str)])
	request = Json.parse(input)
	match request {
		Err(_) => crash "encode did not receive the original request"
		Ok({ id, show_result, .. }) => Json.to_str({
			id,
			input,
			result: if show_result response.result else "",
			status: Str.inspect(response.status),
		})
	}
}
