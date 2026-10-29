# repro for https://github.com/roc-lang/roc/issues/12608
# `missing` is not in scope. `roc check` reports that name error; the `?` on
# the erroneous operand in a function that passes a literal to the custom
# `from_numeral` type must not crash post-check lowering of the `expect`.
Level :: U8.{
	from_numeral : Numeral -> Try(Level, [InvalidNumeral(Str)])
	from_numeral = |_numeral| Ok(Level.(0))
}

check : Level -> {}
check = |_level| {}

run : {} -> Try({}, [Oops])
run = |{}| {
	_ = check(1)
	missing?
	Ok({})
}

expect run({}).is_ok()
