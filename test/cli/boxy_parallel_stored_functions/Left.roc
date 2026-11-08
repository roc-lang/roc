Left := [].{
	apply : I64 -> I64
	apply = |n| (saved.run)(n)
}

make = |captured, step| { run: |n| step(captured, n) }
base : I64
base = 17
saved = make(base, |a, b| a + b)

expect Left.apply(3) == 20
