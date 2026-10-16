# The default for `items` is a `List(U64)`, which decides `a` for every
# `Config(a)`; the tuple access `pair.0` only learns that once `pair`'s type
# is known. The default is rejected, and constructing a `Config` that omits
# `items` crashes.
Config(a) := {
	items : List(a) ?? List.map([(42.U64, "default")], |pair| pair.0),
	label : Str,
}

main! = |_args| {
	echo!("before")
	cfg : Config(Str)
	cfg = Config.{ label: "hello" }
	echo!(Str.join_with(cfg.items, ", "))
	Ok({})
}
