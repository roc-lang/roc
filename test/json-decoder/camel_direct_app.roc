app [main!] { pf: platform "./platform/main.roc" }

import pf.Json

main! : Str => U64
main! = |json| {
	result : Try({ cache_control : Str, user_id : Str }, Json.DecodeErr)
	result = Json.parse_camel(json)

	match result {
		Ok(decoded) =>
			Str.count_utf8_bytes(decoded.cache_control)
				+ Str.count_utf8_bytes(decoded.user_id)

		Err(_) => 999999
	}
}
