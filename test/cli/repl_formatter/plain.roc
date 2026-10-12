module [decode, encode]

import Decoration

decode : Str -> [Evaluate(Str), Reply(Str)]
decode = |line| if line == "ping" Reply("pong") else Evaluate(line)

encode : Str, { status : [Success, Incomplete, Failure], result : Str, stdout : Str, diagnostics : Str, value : [Other, Tag({ name : Str, payload : Try(Str, {}) })] } -> Str
encode = |_input, response| {
	if response.status == Success {
		match response.value {
			Other => Decoration.wrap(response.result)
			Tag({ name, payload }) => match payload {
				Ok(text) => "tag ${name}: ${text}"
				Err({}) => "tag ${name}: not a string"
			}
		}
	} else {
		"ERROR"
	}
}
