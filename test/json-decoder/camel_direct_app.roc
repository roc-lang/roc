app [main!] { pf: platform "./platform/main.roc" }

import pf.Json

parse_direct_record : Str -> Try({ cache_control : Str, user_id : Str }, Json.DecodeErr)
parse_direct_record = Json.parser_camel()

main! : Str => U64
main! = |json| {
	result : Try({ cache_control : Str, user_id : Str }, Json.DecodeErr)
	result = parse_direct_record(json)

	match result {
		Ok(decoded) =>
			Str.count_utf8_bytes(decoded.cache_control)
				+ Str.count_utf8_bytes(decoded.user_id)

		Err(_) => 999999
	}
}
