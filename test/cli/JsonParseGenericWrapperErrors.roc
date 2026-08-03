JsonParseGenericWrapperErrors :: [].{}

parse_response = |body|
	match Json.parse(body) {
		Ok(value) => Ok(value)
		Err(_) => Err(BadResponse)
	}

expect {
	result : Try({ id : I32 }, [BadResponse])
	result = parse_response("{\"id\":1}")

	result == Ok({ id: 1 })
}

expect {
	result : Try({ id : I32 }, [BadResponse])
	result = parse_response("{}")

	result == Err(BadResponse)
}
