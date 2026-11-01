# repro for https://github.com/roc-lang/roc/issues/12767
a = {
	T := [A].{
		N := [B]
	}
	_t : T
	_t = A
	b
}

b = a

main! = |_| Ok({})
