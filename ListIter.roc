expect {
	iter = [1.I64, 2, 3].iter()
	Iter.fold(iter, 0.I64, |acc, item| acc + item) == 6.I64
}

expect {
	iter = Iter.map([10.I64, 20].iter(), |item| item + 1)
	Iter.fold(iter, 0.I64, |acc, item| acc + item) == 32.I64
}
