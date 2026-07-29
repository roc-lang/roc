JsonTagUnionProtocol :: [].{}

Parsed(a) : Try(a, [InvalidJson(Str), MissingRequiredField(Str)])

## One tag union covers every payload arity, so these tests share a single
## derived parser instead of deriving one shape apiece.
Tagged : [Nought, One(U64), Pair(U64, Str), Triple(U64, Str, Bool)]

## An unannotated parsed tag union stays open until equality constrains and
## identifies its known tag. Derived parser dispatch closes that inferred row.
expect Ok(Friendly) == Json.parse("\"Friendly\"")

## Multi-payload tags are a fixed-arity sequence, so they round-trip through
## the same tuple methods, while payload-free and single-payload tags keep
## their own shapes.
expect {
	result : Parsed(Tagged)
	result = Json.parse("\"Nought\"")

	result == Ok(Nought)
}

expect {
	result : Parsed(Tagged)
	result = Json.parse("{\"One\":7}")

	result == Ok(One(7))
}

expect {
	result : Parsed(Tagged)
	result = Json.parse("{\"Pair\":[1,\"a\"]}")

	result == Ok(Pair(1, "a"))
}

expect {
	result : Parsed(Tagged)
	result = Json.parse("{\"Pair\":[1]}")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed(Tagged)
	result = Json.parse("{\"Triple\":[1,\"a\",true,false]}")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	value : Tagged
	value = Pair(1, "a")

	Str.is_eq(Json.to_str(value), "{\"Pair\":[1,\"a\"]}")
}

## A tag union inside a dict is reached through the dict's key and value
## shapes, never through the dict's own private representation.
expect {
	result : Parsed(Dict([Red, Green], U64))
	result = Json.parse("{\"Red\":1,\"Green\":2}")

	match result {
		Ok(d) => Dict.get(d, Red) == Ok(1) and Dict.get(d, Green) == Ok(2)
		Err(_) => False
	}
}

expect {
	result : Parsed(Dict([Red, Green], U64))
	result = Json.parse("{\"Purple\":1}")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed(Dict(Str, Tagged))
	result = Json.parse("{\"a\":{\"One\":7}}")

	match result {
		Ok(d) => Dict.get(d, "a") == Ok(One(7))
		Err(_) => False
	}
}

expect {
	result : Parsed(Set(Tagged))
	result = Json.parse("[{\"One\":7}]")

	match result {
		Ok(s) => Set.len(s) == 1
		Err(_) => False
	}
}

## Bool keys go through parse_key_bool / encode_key_bool rather than being
## routed through Str by the driver.
expect {
	result : Parsed(Dict(Bool, Str))
	result = Json.parse("{\"true\":\"y\",\"false\":\"n\"}")

	match result {
		Ok(d) => Dict.get(d, True) == Ok("y") and Dict.get(d, False) == Ok("n")
		Err(_) => False
	}
}

expect {
	d : Dict(Bool, Str)
	d = Dict.from_list([(True, "y")])

	Str.is_eq(Json.to_str(d), "{\"true\":\"y\"}")
}

expect {
	d : Dict([Red, Green], U64)
	d = Dict.from_list([(Red, 1)])

	Str.is_eq(Json.to_str(d), "{\"Red\":1}")
}
