# repro for https://github.com/roc-lang/roc/issues/12767
x = return {
	T := [A].{
		N := [B]
	}
	_t : T
	_t = A
	1
}

main! = |_| Ok({})
