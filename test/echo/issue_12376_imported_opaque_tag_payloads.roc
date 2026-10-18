# repro for https://github.com/roc-lang/roc/issues/12376
# Derived parsers for tag unions whose payloads reach the same record through
# several imported opaque types, share field labels across records, or carry
# an empty record. The input depends on the command line so each parse runs
# at runtime rather than being evaluated during compilation.
import Issue12376Inner
import Issue12376Outer

Payload : [Direct(Issue12376Inner), Nested(Issue12376Outer), Other({ id : Str, user_id : U64 }), Empty({})]

describe : Try(Payload, _) -> Str
describe = |result|
	match result {
		Ok(Direct(_)) => "direct"
		Ok(Nested(_)) => "nested"
		Ok(Other(other)) => "other ${other.id} ${other.user_id.to_str()}"
		Ok(Empty(_)) => "empty"
		Err(_) => "err"
	}

main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")

	none : Try(Payload, _)
	none = Json.parse(suffix)
	echo!("${describe(none)}\n")

	direct : Try(Payload, _)
	direct = Json.parse(Str.concat("{\"Direct\": {\"id\": 1}}", suffix))
	echo!("${describe(direct)}\n")

	nested : Try(Payload, _)
	nested = Json.parse(Str.concat("{\"Nested\": {\"inner\": {\"id\": 2}, \"user_id\": \"u\"}}", suffix))
	echo!("${describe(nested)}\n")

	other : Try(Payload, _)
	other = Json.parse(Str.concat("{\"Other\": {\"id\": \"o\", \"user_id\": 3}}", suffix))
	echo!("${describe(other)}\n")

	empty : Try(Payload, _)
	empty = Json.parse(Str.concat("{\"Empty\": {}}", suffix))
	echo!("${describe(empty)}\n")

	parse_camel : Str -> Try(Payload, _)
	parse_camel = Json.parser_camel()
	camel = parse_camel(Str.concat("{\"Other\": {\"id\": \"c\", \"userId\": 4}}", suffix))
	echo!("${describe(camel)}\n")
	camel_nested = parse_camel(Str.concat("{\"Nested\": {\"inner\": {\"id\": 5}, \"userId\": \"v\"}}", suffix))
	echo!("${describe(camel_nested)}\n")

	Ok({})
}
