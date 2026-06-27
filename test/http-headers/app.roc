app [main!] { pf: platform "./platform/main.roc" }

Shape : {
	cache_control : Str,
	content_length : U64,
	explicit_optional : Try(Str, [Missing]),
	foo : Str,
	question_optional : Try(Str, [Missing]),
	request_count : U64,
	wildcard_optional : Try(Str, [Missing]),
	x_auth_token : Try(Str, [Missing]),
}

parse_headers : Str -> Try(
	{
		cache_control : Str,
		content_length : U64,
		explicit_optional : Try(Str, [Missing]),
		foo : Str,
		question_optional : Try(Str, [Missing]),
		request_count : U64,
		wildcard_optional : Try(Str, [Missing]),
		x_auth_token : Try(Str, [Missing]),
	},
	Encoding.HttpHeader,
)
parse_headers = Encoding.HttpHeader.parser_for()

main! : Str => U64
main! = |headers| {
	decoded_result : Try(
		{
			cache_control : Str,
			content_length : U64,
			explicit_optional : Try(Str, [Missing]),
			foo : Str,
			question_optional : Try(Str, [Missing]),
			request_count : U64,
			wildcard_optional : Try(Str, _),
			x_auth_token : Try(Str, [Missing]),
		},
		Encoding.HttpHeader,
	)
	decoded_result = parse_headers(headers)

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

			x_auth_token_length = match decoded.x_auth_token {
				Ok(value) => Str.count_utf8_bytes(value)
				Err(Missing) => 0
			}

			decoded.content_length
				+ decoded.request_count
				+ Str.count_utf8_bytes(decoded.cache_control)
				+ Str.count_utf8_bytes(decoded.foo)
				+ explicit_optional_length
				+ wildcard_optional_length
				+ question_optional_length
				+ x_auth_token_length
		}
		Err(Encoding.HttpHeader.MissingRequired) => 999999
		Err(Encoding.HttpHeader.BadHeader) => 999999
	}
}

question_length : Try(Str, [Missing]) -> Try(U64, [Missing])
question_length = |maybe| {
	value = maybe?
	Ok(Str.count_utf8_bytes(value))
}
