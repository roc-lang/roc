foo : List(U8) -> Try({}, [ParsingIncomplete([MyTag]), ..])
foo = |bytes| {
	s = Json.parse(Str.from_utf8(bytes)?)?
	Ok(s)
}

main! = |_args| {
	_ = foo([])?
	Ok({})
}
