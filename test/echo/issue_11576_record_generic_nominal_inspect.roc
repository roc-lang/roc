# repro for https://github.com/roc-lang/roc/issues/11576
#
# Records whose fields are nominals with type parameters (a custom
# `to_inspect` nominal, `Dict`, `Set`) are inspected under every backend,
# including `--specialize=no`.
Wrap(a) := [W(a)].{
	to_inspect : Wrap(a) -> Str
	to_inspect = |Wrap.W(value)| "Wrap(${Str.inspect(value)})"
}

render = |v| Str.inspect({ value: v })

main! = |args| {
	s = args.first() ?? "x"
	w : Wrap(Str)
	w = W(s)
	echo!("${Str.inspect({ w: w, l: [w] })}\n")
	echo!("${render(w)}\n")
	d = Dict.from_list([("one", 1.I64)])
	set = Set.from_list([1.I64, 2])
	echo!("${Str.inspect({ d: d, s: set })}\n")
	Ok({})
}
