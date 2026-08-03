JsonParseInferredRecord :: [].{}

expect {
	parsed = Json.parse("{\"first\":\"one\",\"second\":\"two\"}")

	match parsed {
		Ok(decoded) =>
			decoded.first == "one" and decoded.second == "two"

		Err(_) =>
			False
		}
}
