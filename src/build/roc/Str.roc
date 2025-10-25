Str := [ProvidedByCompiler].{
	is_empty : Str -> Bool
	is_empty = |_str| False

	contains : Str, Str -> Bool
	contains = |_str, _other| True
}
