Shapes := [].{
	scale : U64 -> Box(U64 -> U64)
	scale = |k| Box.box(|x| x * k)

	label : U64 -> Str
	label = |n| {
		parts = { name: "n", value: n, extra: (n, [n, n + 1]) }
		"${parts.name}=${parts.value.to_str()}/${parts.extra.1.len().to_str()}"
	}
}
