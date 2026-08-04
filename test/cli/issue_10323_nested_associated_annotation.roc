# repro for https://github.com/roc-lang/roc/issues/10323
f : () -> () where [a.n : I]
f = {
	[]
	{}
	U := [].{
		e : U
		B := [].{
			I := [].{
				g = 0
			}
		}
	}
}
