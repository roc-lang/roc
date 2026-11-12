S := [].{
	wrap : Str -> Str
	wrap = |s| "(${s})"
}

expect S.wrap("a") == "(a)"
