app [main!] { pf: platform "./platform/main.roc" }

parse_camel_record : Str -> Try(
	{
		cache_control : Str,
		nested_record : { inner_value : Str },
		pair : [Pair({ first_value : Str, second_value : Str })],
		user_id : Str,
	},
	Json,
)
parse_camel_record = Json.parser_camel()

main! : Str => U64
main! = |json| {
	result = parse_camel_record(json)

	match result {
		Ok(decoded) => {
			pair_length = match decoded.pair {
				Pair(payload) =>
					Str.count_utf8_bytes(payload.first_value)
						+ Str.count_utf8_bytes(payload.second_value)
			}

			Str.count_utf8_bytes(decoded.cache_control)
				+ Str.count_utf8_bytes(decoded.nested_record.inner_value)
				+ Str.count_utf8_bytes(decoded.user_id)
				+ pair_length
		}

		Err(_) => 999999
	}
}
