# `Level` wraps a `U8` but declares no methods, so the numerals cannot convert
# to it and `List.sum` cannot add or start from it. The literals themselves are
# rejected, not the `sum` call that relates them to `Level`, and reading
# `total` crashes.
Level := U8

total : Level
total = [1, 2].sum()

main! = |_args| {
	echo!("before")
	echo!(Str.inspect(total))
	Ok({})
}
