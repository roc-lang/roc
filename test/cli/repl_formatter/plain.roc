module [decode, encode]

import Decoration

decode : Str -> Try(Str, Str)
decode = |line| if line == "ping" Err("pong") else Ok(line)

encode : { result : Str, stdout : Str, diagnostics : Str } -> Str
encode = |response| {
	if response.diagnostics.is_empty() {
		Decoration.wrap(response.result)
	} else {
		"ERROR"
	}
}
