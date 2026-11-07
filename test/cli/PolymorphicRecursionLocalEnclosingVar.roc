## A local annotation's type variable that names the enclosing annotation's
## is the enclosing definition's variable, so the local cannot recur at
## another type for it.
PolymorphicRecursionLocalEnclosingVar := [].{
	run : a, U64 -> U64
	run = |x, n| {
		go : a, U64 -> U64
		go = |_y, k| if k == 0 { 0 } else { 1 + go("s", k - 1) }
		go(x, n)
	}
}

expect PolymorphicRecursionLocalEnclosingVar.run(1.U8, 2) == 2
