# A recursive nominal's checked template and a constant built from it share
# one checked root, whose row payload is flat however the solver stored the
# row (`[End] ext [Next(Px)]` or `[End, Next(Px)]`), so restoring the constant
# finds every tag it names.
Px := [End, Next(Px)].{
	is_eq : _
	from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
	from_numeral = |_numeral| Ok(Next(End))
}

two : Px
two = Px.Next(Px.Next(Px.End))

depth : Px -> U64
depth = |p| match p {
	End => 0
	Next(q) => 1 + depth(q)
}

matches : Px -> Bool
matches = |p| match p {
	0 => True
	_ => False
}

main! = |_args| {
	echo!("${U64.to_str(depth(two))} ${Str.inspect(matches(Px.Next(Px.End)))} ${Str.inspect(matches(Px.End))}\n")
	Ok({})
}
