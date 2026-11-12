# Each function below calls itself and has a `_` in its annotation, so part of
# its type is inferred from its body. What the body puts there has a
# requirement of its own: a literal's conversion in `count_down` and `pick`,
# and a `to_str` method in `show_first` and `show_last`. Each recursive call
# passes that requirement along unchanged.

# The string in `Seven("seven")` is a literal in the inferred error type.
count_down : U64 -> Try(U64, _)
count_down = |n| if n == 7 Err(Seven("seven")) else if n == 0 Ok(n) else count_down(n - 1)

# The `5` is a literal of the inferred result type.
pick : U64 -> _
pick = |n| if n == 0 5 else pick(n - 1)

# `x.to_str()` is checked before the recursive call.
show_first : U64, _ -> Str
show_first = |n, x| if n == 0 x.to_str() else show_first(n - 1, x)

# `x.to_str()` is checked after the recursive call.
show_last : U64, _ -> Str
show_last = |n, x| if n != 0 show_last(n - 1, x) else x.to_str()

main! = |args| {
	# A runtime zero, so the calls below run when the program does.
	zero = args.len() * 0
	stopped = match count_down(zero + 9) {
		Ok(_) => "reached zero"
		Err(Seven(text)) => text
	}
	reached = match count_down(zero + 3) {
		Ok(n) => n.to_str()
		Err(Seven(text)) => text
	}
	small : U8
	small = pick(zero + 2)
	exact : Dec
	exact = pick(zero + 2)
	echo!("${stopped} ${reached} ${small.to_str()} ${exact.to_str()} ${show_first(zero + 2, 7.I16)} ${show_last(zero + 2, 8.U32)}")
	Ok({})
}
