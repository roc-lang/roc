Blub := [].{
	Problem : BlubProblem
}

BlubProblem := [TooBig, TooSmall].{
	message : BlubProblem -> Str
	message = |_| "x"

	extra : BlubProblem -> Str
	extra = |_| "y"
}
