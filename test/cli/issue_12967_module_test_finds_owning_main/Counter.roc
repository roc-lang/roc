import pf.Wrap

Counter := [].{
	label : U64 -> Str
	label = |n| Wrap.wrap(n.to_str())
}

expect Counter.label(4) == "(4)"
