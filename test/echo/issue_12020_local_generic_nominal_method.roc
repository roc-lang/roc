# A generic nominal type declared inside a function body, whose annotated
# method applies the type to its own type variable. Every use of the local
# declaration must instantiate its generalized template, so the method's
# `Label(a)` connects the field type to the method's result.
make_label = |value| {
	Label(a) := { text : a }.{
		render : Label(a) -> a
		render = |label| label.text
	}

	Label.render(Label.{ text: value })
}

make_pair = |left, right| {
	Pair(a) := { first : a, second : a }

	second : Pair(b) -> b
	second = |pair| pair.second

	second(Pair.{ first: left, second: right })
}

main! = |_args| {
	label = make_label(42.I64)
	echo!("label: ${Str.inspect(label)}")
	echo!("text: ${make_label("hi")}")
	echo!("pair: ${make_pair("a", "b")}")
	Ok({})
}
