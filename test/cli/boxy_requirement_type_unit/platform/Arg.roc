Arg := [Arg(Str)].{
	from_str : Str -> Arg
	from_str = |text| Arg.Arg(text)

	to_inspect : Arg -> Str
	to_inspect = |Arg.Arg(text)| "Arg(${text})"
}
