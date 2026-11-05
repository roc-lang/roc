# A method defined as another method of the same type, with its own annotation
# carrying a `where` clause, called by its own name.
Annotated(a) := { v : a }.{
	scaled : Annotated(a) -> a where [a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]
	scaled = |s| s.v * 2

	twice : Annotated(a) -> a where [a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]
	twice = scaled
}

mk : U16 -> Annotated(U16)
mk = |n| { v: n }

main! = |_args| {
	echo!(Annotated.twice(mk(4)).to_str())
	Ok({})
}
