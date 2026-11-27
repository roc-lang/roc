Lst := [Nil, Cons(I64, Lst)]

build : I64 -> Lst
build = |n| if n <= 0 { Nil } else { Cons(n, build(n - 1)) }

sum : Lst, I64 -> I64
sum = |l, acc| match l {
	Nil => acc
	Cons(h, t) => sum(t, acc + h)
}

expect sum(build(4), 0) == 10
