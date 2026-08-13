# A defaulted tag-union field follows polarity through an alias body exactly
# as it does written inline (design.md "Polarity" / "Defaulted Fields"): the
# alias declaration stores the ordinary polarity marker for the field's
# extensionless union, and the concreteness judgment admits it. This used to
# reject with DEFAULT VALUE NOT CONCRETE while the identical inline
# annotation was accepted.
Config : { mode : [Fast, Slow] ?? Fast }

mk : U64 -> Config
mk = |_n| {}

go : U64 -> U64
go = |n| {
	c = mk(n)
	match c.mode {
		Fast => n
		Slow => n + 1
	}
}

main! = |args| {
	if go(List.len(args)) < 100 Ok({}) else Err(Exit(1))
}
