# repro for https://github.com/roc-lang/roc/issues/12608
# A rejected `?` operand and a rejected explicit early `return` must not retire
# the `from_numeral` plans of literals elsewhere in their enclosing functions.
LevelReturn :: U8.{
	from_numeral : Numeral -> Try(LevelReturn, [InvalidNumeral(Str)])
	from_numeral = |_numeral| Ok(LevelReturn.(0))
}

check : LevelReturn -> {}
check = |_level| {}

mismatched : {} -> Try({}, [Oops])
mismatched = |{}| {
	_ = check(1)
	"a"?
	Ok({})
}

explicit : {} -> Try({}, [Oops])
explicit = |{}| {
	_ = check(2)
	match missing {
		Ok(_) => {}
		Err(e) => return Err(e)
	}
	Ok({})
}

expect mismatched({}).is_ok()

expect explicit({}).is_ok()
