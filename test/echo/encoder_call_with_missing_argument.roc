# Each call below passes an argument that does not exist, so the call is
# retired for that argument: it evaluates its earlier arguments and then
# crashes. The encoder the call would have selected never runs, so the only
# report is that the argument does not exist; nothing reports the encoder's
# type as undetermined.
describe_both : Str, a -> Str where [a.encoder_for : _]
describe_both = |label, value| Str.concat(label, Json.to_str(value))

encoded = Json.to_str(Str.nope("foo.json"))

described = describe_both("label", Str.nope("foo.json"))

main! = |args| {
	echo!("before")
	if List.len(args) > 100 {
		echo!(described)
	}
	echo!(encoded)
	echo!("after")
	Ok({})
}
