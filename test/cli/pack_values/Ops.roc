# Closed functions a program reaches both by direct call and as values, so
# the object cache serves the plain procedure and the erased entries that
# forward to it.
Ops :: [].{
	double : U64 -> U64
	double = |n| n * 2

	triple : U64 -> U64
	triple = |n| n * 3

	clamp : U64, U64 -> U64
	clamp = |n, max| if n > max { max } else { n }
}
