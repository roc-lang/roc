foo : {} -> Try({}, [ErrTag([CustomType]), ..])
foo = |_| Err(ErrTag([CustomType]))

main! = |_args| {
	_ = foo({})?
	Ok({})
}
