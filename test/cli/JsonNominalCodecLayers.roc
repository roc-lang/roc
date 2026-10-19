JsonNominalCodecLayers :: [].{}

Secret :: Str.{
	encoder_for : encoding -> (Secret, state -> Try(state, err))
		where [
			encoding.encode_str : Str, state -> Try(state, err),
		]
	encoder_for = |_encoding| {
		Encoding : encoding

		|_secret, state| Encoding.encode_str("***", state)
	}
}

Tagged :: Str.{
	parser_for : encoding -> (state -> Try({ value : Tagged, rest : state }, err))
		where [
			encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err),
		]
	parser_for = |encoding| {
		Encoding : encoding

		|state| {
			parsed = Encoding.parse_str(encoding, state)?
			Ok({ value: Tagged.(Str.concat("tagged:", parsed.value)), rest: parsed.rest })
		}
	}
	encoder_for : _
}

Username := Str.{
	parser_for : _
	encoder_for : _
}

Flag :: Bool.{ encoder_for : _ }

Login := Username.{
	parser_for : _
	encoder_for : _
}

Account := Login.{
	parser_for : _
	encoder_for : _
}

Point :: { x : I64, y : I64 }.{ encoder_for : _ }

Location :: Point.{ encoder_for : _ }

Hidden :: Secret.{ encoder_for : _ }

Label :: Tagged.{
	parser_for : _
	encoder_for : _
}

expect Json.to_str({ token: Secret.("hunter2") }) == "{\"token\":\"***\"}"
expect Json.to_str((Secret.("a"), [Secret.("b")])) == "[\"***\",[\"***\"]]"
expect Json.to_str({ name: Username.("ada"), on: Flag.(Bool.True) }) == "{\"name\":\"ada\",\"on\":true}"
expect Json.to_str(Account.(Login.(Username.("ada")))) == "\"ada\""
expect Json.to_str({ accounts: [Account.(Login.(Username.("ada")))] }) == "{\"accounts\":[\"ada\"]}"
expect Json.to_str({ at: Location.(Point.({ x: 1, y: 2 })) }) == "{\"at\":{\"x\":1,\"y\":2}}"
expect Json.to_str({ hidden: Hidden.(Secret.("hunter2")) }) == "{\"hidden\":\"***\"}"

account_round_trips : Str -> Bool
account_round_trips = |json| {
	parsed : Try({ account : Account }, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse(json)

	match parsed {
		Ok(value) => Json.to_str(value) == json
		Err(_) => False
	}
}

expect account_round_trips("{\"account\":\"ada\"}")

label_uses_inner_parser : Str -> Bool
label_uses_inner_parser = |json| {
	parsed : Try({ label : Label }, [InvalidJson(Str), MissingRequiredField(Str)])
	parsed = Json.parse(json)

	match parsed {
		Ok(value) => Json.to_str(value) == "{\"label\":\"tagged:x\"}"
		Err(_) => False
	}
}

expect label_uses_inner_parser("{\"label\":\"x\"}")
