# `undefined_fn` and `undefined_tag` are not in scope, so each `Err` below is
# an erroneous value. It already owns its report, so it is not related to the
# closed `[Blue]` result and adds no type mismatch of its own. The program runs
# until `returned` reaches its `return`.
returned : I64 -> [Blue]
returned = |x| {
	if x > 0 {
		return Err(undefined_fn(x))
	}
	Blue
}

branched : I64 -> [Blue]
branched = |x| if x > 0 Err(undefined_tag(x)) else Blue

main! = |args| {
	echo!("before")
	n = if List.len(args) > 100 1 else 0
	_ = returned(n)
	_ = branched(n)
	echo!("middle")
	_ = returned(1)
	echo!("after")
	Ok({})
}
