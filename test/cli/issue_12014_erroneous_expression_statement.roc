# An expression statement wrapping an undefined name has an erroneous value
# type. Checking reports only the undefined name and replaces the statement
# with a runtime error, so `roc check` and `roc` do not crash while lowering it.

parse_name = |text| {
	Err(InvalidName(txt)) # typo in the variable name
	Ok(text)
}

main! = |_| {
	echo!("before")
	_ = parse_name("Ann")
	Ok({})
}
