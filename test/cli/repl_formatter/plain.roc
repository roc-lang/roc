module [decode, encode]

import Decoration

decode : Str -> Try(Str, Str)
decode = |line| if line == "ping" Err("pong") else Ok(line)

encode : { result : Str, stdout : Str, diagnostics : Str, value : [Other, Tag({ name : Str, payload : Try(Str, {}) })] } -> Str
encode = |response| {
	if response.diagnostics.is_empty() {
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
