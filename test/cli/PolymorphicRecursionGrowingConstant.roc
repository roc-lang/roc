# A top-level constant calls an annotated function whose recursive call
# instantiates its own type variable at a strictly larger type (`a` at
# `List(a)`). `roc check` evaluates the constant, so it must finish and report
# the recursive call instead of specializing `depth` forever.
PolymorphicRecursionGrowingConstant := [].{
	depth : a, U64 -> U64
	depth = |x, n| if n == 0 { 0 } else { 1 + depth([x], n - 1) }

	three : U64
	three = depth({}, 3)
}
