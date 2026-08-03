JsonNominalScalarRoundTrip :: [].{}

Username := Str.{
	encoder_for : _
	parser_for : _
}

Age := I64.{
	encoder_for : _
	parser_for : _
}

# A refined string encodes as its backing string, not as `{}`.
expect {
	value : Username
	value = Username.("ada")

	Str.is_eq(Json.to_str(value), "\"ada\"")
}

# A refined integer encodes as its backing integer.
expect {
	value : Age
	value = Age.(42)

	Str.is_eq(Json.to_str(value), "42")
}

# The same holds for a refined field inside a record.
expect {
	value : { name : Username, age : Age }
	value = { name: Username.("a heap allocated username value"), age: Age.(7) }

	Str.is_eq(Json.to_str(value), "{\"age\":7,\"name\":\"a heap allocated username value\"}")
}

# Round-trip: parsing a refined string yields the nominal, which re-encodes to the input.
# `u_to_str`'s signature forces the parse target to `Username` without naming the error type.
u_to_str : Username -> Str
u_to_str = |name| Json.to_str(name)

username_round_trips : Str -> Bool
username_round_trips = |json|
	match Json.parse(json) {
		Ok(name) => Str.is_eq(u_to_str(name), json)
		Err(_) => Bool.False
	}

expect username_round_trips("\"a heap allocated username value\"")
