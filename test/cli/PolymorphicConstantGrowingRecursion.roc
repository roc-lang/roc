# Each step of `grow` would read `empty` at a larger type. Checking rejects
# the recursion, and the polymorphic constant's instances neither hide the
# report nor keep the compiler from finishing.
PolymorphicConstantGrowingRecursion := [].{}

empty : List(_a)
empty = []

grow : a, U64 -> U64
grow = |x, n| if n == 0 List.len(List.append(empty, x)) else grow((x, x), n - 1)

expect grow(1.U8, 2) == 1
