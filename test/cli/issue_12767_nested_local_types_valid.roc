# repro for https://github.com/roc-lang/roc/issues/12767
f : {} -> U64
f = |_| {
	U := [Q].{
		B := [R].{
			I := [S].{
				g = 3
				k = [S, S]
			}
		}
	}
	U.B.I.g + List.len(U.B.I.k)
}

v = f({})

expect v == 5

main! = |_| Ok({})
