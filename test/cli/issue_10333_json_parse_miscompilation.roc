main! = |_| {
	input = "{\"body\": {\"a\": [\"hello\"]}}"

	x : { body : { a : List(Str) } }
	x = Json.parse(input)?

	y : { body : Dict(Str, List(Str)) }
	y = Json.parse(input)?

    echo!("${Str.inspect(x)}\n")
    echo!("${Str.inspect(y)}\n")

	Ok({})
}
