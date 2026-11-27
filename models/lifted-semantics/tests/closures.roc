# Selftest program: closures chosen by `if` and `match`, captures, a higher-order function,
# a function returning a closure, a closure in a tag payload, and fuel-bounded recursion.
Op := [Inc(I64), Fn(I64 -> I64)]

apply_twice : (I64 -> I64), I64 -> I64
apply_twice = |f, x| f(f(x))

adder : I64 -> (I64 -> I64)
adder = |n| |x| x + n

pick : Bool, I64 -> (I64 -> I64)
pick = |b, k| if b |x| x * k else adder(k)

run_op : Op, I64 -> I64
run_op = |op, x| match op {
	Inc(k) => x + k
	Fn(g) => g(x)
}

iterate : I64, (I64 -> I64), I64 -> I64
iterate = |fuel, f, x| if fuel <= 0 x else iterate(fuel - 1, f, f(x))

compose : (I64 -> I64), (I64 -> I64) -> (I64 -> I64)
compose = |f, g| |x| g(f(x))

main1 : I64 -> I64
main1 = |n| {
	f = pick(n < 2, 3)
	g = compose(f, adder(n))
	run_op(Fn(g), 1) + run_op(Inc(n), 2) + apply_twice(adder(5), n) + iterate(3, g, n)
}

expect {
	dbg main1(0)
	Bool.True
}

expect {
	dbg main1(4)
	Bool.True
}

expect main1(1) == main1(1)
