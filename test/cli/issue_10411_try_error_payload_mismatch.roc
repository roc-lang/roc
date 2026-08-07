bar : {} -> Try({}, [ErrTag(Str), ..])
bar = |_| Err(ErrTag("err"))

foo : {} -> Try({}, [ErrTag(U64), ..])
foo = |_| {
	_ = bar({})?
	Ok({})
}

main! = |_args| {
	_ = foo({})?
	Ok({})
}
