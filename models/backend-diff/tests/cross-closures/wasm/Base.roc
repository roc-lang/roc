Base :: [].{
	adder : I64 -> (I64 -> I64)
	adder = |k| |y| y + k

	shape : I64 -> [P(I64), Q(Str), W]
	shape = |n| if n < 0 W else if n < 10 P(n) else Q(n.to_str())
}
