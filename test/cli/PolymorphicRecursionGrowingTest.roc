# An annotated function whose recursive call instantiates its own type
# variable at a strictly larger type (`a` at `List(a)`) needs a new
# specialization at every depth: `depth` at `a`, `List(a)`,
# `List(List(a))`, and so on without end. Compilation must finish and report
# the recursive call instead of specializing forever.
PolymorphicRecursionGrowingTest := [].{
	depth : a, U64 -> U64
	depth = |x, n| if n == 0 { 0 } else { 1 + depth([x], n - 1) }
}

expect PolymorphicRecursionGrowingTest.depth({}, 3) == 3
