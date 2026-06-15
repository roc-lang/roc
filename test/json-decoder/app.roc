app [main!] { pf: platform "./platform/main.roc" }

import pf.Json

main! : Json => U64
main! = |json| {
	decoded : {
		explicit_optional : Try(Str, [Missing]),
		foo : Str,
		nested : {
			bar : Str,
			mode : [Warm, Cold],
		},
		question_optional : Try(Str, [Missing]),
		status : [Active, Paused],
		wildcard_optional : Try(Str, _),
	}
	decoded = Json.decode(json, Decoder.derive({}))

	explicit_optional_length = match decoded.explicit_optional {
		Ok(value) => Str.count_utf8_bytes(value)
		Err(Missing) => 0
	}

	wildcard_optional_length = match decoded.wildcard_optional {
		Ok(value) => Str.count_utf8_bytes(value)
		Err(_) => 0
	}

	question_optional_length = match question_length(decoded.question_optional) {
		Ok(length) => length
		Err(Missing) => 0
	}

	status_score = match decoded.status {
		Active => 11
		Paused => 17
	}

	mode_score = match decoded.nested.mode {
		Warm => 19
		Cold => 23
	}

	Str.count_utf8_bytes(decoded.foo)
		+ Str.count_utf8_bytes(decoded.nested.bar)
		+ explicit_optional_length
		+ wildcard_optional_length
		+ question_optional_length
		+ status_score
		+ mode_score
}

question_length : Try(Str, [Missing]) -> Try(U64, [Missing])
question_length = |maybe| {
	value = maybe?
	Ok(Str.count_utf8_bytes(value))
}
