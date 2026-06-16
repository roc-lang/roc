app [main!] { pf: platform "./platform/main.roc" }

import pf.Headers

main! : Str => U64
main! = |headers| {
	decoded_result : Try({
		explicit_optional : Try(Str, [Missing]),
		foo : Str,
		question_optional : Try(Str, [Missing]),
		wildcard_optional : Try(Str, _),
	}, Headers.DecodeErr)
	decoded_result = Headers.decode(headers)

	match decoded_result {
		Ok(decoded) => {

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

			Str.count_utf8_bytes(decoded.foo) + explicit_optional_length + wildcard_optional_length + question_optional_length
		}
		Err(_) => 999999
	}
}

question_length : Try(Str, [Missing]) -> Try(U64, [Missing])
question_length = |maybe| {
	value = maybe?
	Ok(Str.count_utf8_bytes(value))
}
