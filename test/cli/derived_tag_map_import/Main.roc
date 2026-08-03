import Mapped

expect {
	value : Mapped.Maybe(U64)
	value = Mapped.Maybe.Just(42)

	match value.map(U64.to_str) {
		Mapped.Maybe.Just("42") => True
		_ => False
	}
}

expect Mapped.opaque_value(Mapped.opaque_just(42).map(U64.to_str)) == "42"

main! = |_args| Ok({})
