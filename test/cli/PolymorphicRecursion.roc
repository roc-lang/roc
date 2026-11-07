PolymorphicRecursion := {}

# Growth, direct.
depth : U64, a -> U64
depth = |n, x| if n == 0 0 else 1 + depth(n - 1, [x])

# Permutation, direct.
swap : U64, a, b -> U64
swap = |n, x, y| if n == 0 0 else 1 + swap(n - 1, y, x)

# Growth through mutual recursion.
ping : U64, a -> U64
ping = |n, x| if n == 0 0 else pong(n - 1, x)

pong : U64, a -> U64
pong = |n, x| if n == 0 0 else ping(n - 1, [x])

# Growth through a method called with method syntax.
Poly(a) := [Val(a)].{
	size : Poly(a), U64 -> U64
	size = |Poly.Val(x), n| if n == 0 0 else Poly.Val((x, x)).size(n - 1)
}

expect depth(3, "x") == 3
expect swap(3, "x", 1.U8) == 3
expect ping(3, "x") == 3
expect Poly.Val(1.U64).size(3) == 0
