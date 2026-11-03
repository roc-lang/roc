# Json keeps the field names of records of every width, nested and in lists,
# in both directions. `{ name }` with no trailing comma is a block that evaluates
# to `name`, so it encodes as the string itself. `{ name, }` and `{ name: name }`
# are one-field records.
#
# Every value here comes from the program's arguments, so the encoders run in the
# built program and not at compile time.

app [main!] {}

line! = |text| echo!("${text}\n")

One : { name : Str }

Two : { name : Str, age : U64 }

Nested : { outer : { name : Str } }

main! = |args| {
	none = args.drop_first(args.len())
	name = match none.append("a string").first() {
		Ok(value) => value
		Err(_) => "unused"
	}
	age = none.len() + 7

	line!(
		Json.to_str(
			{
				name
			},
		),
	)
	line!(
		Json.to_str([
			{
				name
			},
		]),
	)
	line!(
		Json.to_str({
			name,
		}),
	)
	line!(
		Json.to_str([
			{
				name,
			},
		]),
	)
	line!(Json.to_str({ name: name }))
	line!(Json.to_str([{ name: name }]))
	line!(Json.to_str({ name, age }))
	line!(Json.to_str([{ name, age }]))
	line!(Json.to_str({ outer: { name: name } }))
	line!(Json.to_str([{ outer: [{ name: name }] }]))

	one : Try(One, _)
	one = Json.parse("{\"name\":\"x\"}")
	line!(Str.inspect(one))

	two : Try(Two, _)
	two = Json.parse("{\"age\":3,\"name\":\"x\"}")
	line!(Str.inspect(two))

	nested : Try(Nested, _)
	nested = Json.parse("{\"outer\":{\"name\":\"x\"}}")
	line!(Str.inspect(nested))

	ones : Try(List(One), _)
	ones = Json.parse("[{\"name\":\"x\"},{\"name\":\"y\"}]")
	line!(Str.inspect(ones))

	not_a_record : Try(One, _)
	not_a_record = Json.parse("\"x\"")
	line!(Str.inspect(not_a_record))

	Ok({})
}
