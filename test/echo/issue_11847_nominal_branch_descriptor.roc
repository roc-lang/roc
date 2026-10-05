# repro for https://github.com/roc-lang/roc/issues/11847
#
# Returning a nominal's parameter value from one match branch and a constructed
# value from the other transfers the shared nominal and backing descriptor once.
T(a) := [Empty, Full(a)]

pick : T(a), T(a) -> T(a)
pick = |x, y| {
	match y {
		Empty => x
		Full(v) => Full(v)
	}
}

show : T(Str) -> Str
show = |t| match t {
	Empty => "empty"
	Full(s) => s
}

main! = |args| {
	y : T(Str)
	y = if args.is_empty() { Empty } else { Full("arg") }
	a = pick(Full("x"), y)
	b = pick(Empty, Full("z"))
	c : T(Str)
	c = pick(Empty, Empty)
	echo!("${show(a)} ${show(b)} ${show(c)}\n")
	Ok({})
}
