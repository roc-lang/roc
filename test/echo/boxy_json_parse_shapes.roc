# Derived JSON parsers drive every container shape through the format's parse
# protocol. The input depends on the command line so each parse runs at
# runtime rather than being evaluated during compilation.
main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")

	empty : Try({}, _)
	empty = Json.parse(Str.concat("{}", suffix))
	echo!("${Str.inspect(empty)}\n")

	person : Try({ name : Str, age : U64 }, _)
	person = Json.parse(Str.concat("{\"age\": 36, \"skipped\": [1, {\"a\": null}], \"name\": \"ada\"}", suffix))
	echo!("${Str.inspect(person)}\n")

	missing : Try({ name : Str, age : U64 }, _)
	missing = Json.parse(Str.concat("{\"name\": \"ada\"}", suffix))
	echo!("${Str.inspect(missing)}\n")

	pair : Try((Str, U64), _)
	pair = Json.parse(Str.concat("[\"a\", 2]", suffix))
	echo!("${Str.inspect(pair)}\n")

	nested : Try(List(List(U64)), _)
	nested = Json.parse(Str.concat("[[1, 2], [], [3]]", suffix))
	echo!("${Str.inspect(nested)}\n")

	colors : Try(List([Red, Green, Rgb(U8, U8, U8)]), _)
	colors = Json.parse(Str.concat("[\"Green\", {\"Rgb\": [1, 2, 3]}, \"Red\"]", suffix))
	echo!("${Str.inspect(colors)}\n")

	Ok({})
}
