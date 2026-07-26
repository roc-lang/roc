JsonContainerProtocol :: [].{}

Parsed(a) : Try(a, [InvalidJson(Str), MissingRequiredField(Str)])

## Tuple arity is static, so the format itself rejects a JSON array whose
## length disagrees rather than the driver manufacturing a generic error.
expect {
	result : Parsed((U64, Str))
	result = Json.parse("[1,\"a\"]")

	result == Ok((1, "a"))
}

expect {
	result : Parsed((U64, Str))
	result = Json.parse("[1]")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed((U64, Str))
	result = Json.parse("[1,\"a\",2]")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed((U64, Str))
	result = Json.parse("[]")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

## A list and a tuple both read a JSON array, but a list accepts any length.
expect {
	result : Parsed(List(U64))
	result = Json.parse("[]")

	result == Ok([])
}

expect {
	result : Parsed(List(U64))
	result = Json.parse("[1,2,3]")

	result == Ok([1, 2, 3])
}

expect {
	result : Parsed({ a : List(U64), b : (Str, U64) })
	result = Json.parse("{\"a\":[7,8],\"b\":[\"x\",9]}")

	result == Ok({ a: [7, 8], b: ("x", 9) })
}

expect {
	result : Parsed({})
	result = Json.parse("{}")

	match result {
		Ok(_) => True
		Err(_) => False
	}
}

## Trailing separators are the format's business at every container position,
## so the same input is rejected by default and accepted under TrailingCommas.
expect {
	result : Parsed(List(U64))
	result = Json.parse("[1,2,]")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed(List(U64))
	result = Json.parse_trailing_commas("[1,2,]")

	result == Ok([1, 2])
}

expect {
	result : Parsed({ a : U64 })
	result = Json.parse("{\"a\":1,}")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed({ a : U64 })
	result = Json.parse_trailing_commas("{\"a\":1,}")

	result == Ok({ a: 1 })
}

expect {
	result : Parsed((U64, Str))
	result = Json.parse("[1,\"a\",]")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed((U64, Str))
	result = Json.parse_trailing_commas("[1,\"a\",]")

	result == Ok((1, "a"))
}

expect {
	result : Parsed(Dict(Str, U64))
	result = Json.parse_trailing_commas("{\"a\":1,}")

	match result {
		Ok(d) => Dict.get(d, "a") == Ok(1)
		Err(_) => False
	}
}

## Dict keys go through the key type's own parser, so a non-Str key type reads
## the quoted text as that type instead of being routed through Str.
expect {
	result : Parsed(Dict(U64, Str))
	result = Json.parse("{\"7\":\"seven\",\"8\":\"eight\"}")

	match result {
		Ok(d) => Dict.get(d, 7) == Ok("seven") and Dict.get(d, 8) == Ok("eight")
		Err(_) => False
	}
}

expect {
	result : Parsed(Dict(U64, Str))
	result = Json.parse("{\"notanumber\":\"x\"}")

	match result {
		Ok(_) => False
		Err(_) => True
	}
}

expect {
	result : Parsed(Dict(Str, List(U64)))
	result = Json.parse("{\"a\":[1,2],\"b\":[]}")

	match result {
		Ok(d) => Dict.get(d, "a") == Ok([1, 2]) and Dict.get(d, "b") == Ok([])
		Err(_) => False
	}
}

expect {
	d : Dict(U64, Str)
	d = Dict.from_list([(3, "v")])

	Str.is_eq(Json.to_str(d), "{\"3\":\"v\"}")
}

expect {
	d : Dict(Str, List(U64))
	d = Dict.from_list([("a", [1, 2])])

	Str.is_eq(Json.to_str(d), "{\"a\":[1,2]}")
}

expect {
	value : (U64, Str)
	value = (1, "a")

	Str.is_eq(Json.to_str(value), "[1,\"a\"]")
}

## Records round-trip through parse_record_start / parse_record_after_field
## with nested containers at every position.
expect {
	first : Parsed({ id : U64, tags : List(Str), pair : (U64, Str) })
	first = Json.parse("{\"id\":7,\"tags\":[\"a\",\"b\"],\"pair\":[1,\"z\"]}")

	match first {
		Ok(value) => {
			second : Parsed({ id : U64, tags : List(Str), pair : (U64, Str) })
			second = Json.parse(Json.to_str(value))

			second == Ok(value)
		}

		Err(_) => False
	}
}
