Lib := [].{
	constant : List(U8)
	constant = [10, 20]

	wrap : U64 -> List(List(U8))
	wrap = |n|
		if n == 0 {
			[Lib.constant]
		} else {
			Lib.wrap(n - 1)
		}
}
