RecursiveEqualityLocal := {}

compare_with = |expected, value| {
	Expr := [Leaf(Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left), Leaf(right)) => left == expected and right == expected
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf(value)) == Expr.Next(Expr.Leaf(value))
}

expect compare_with("a", "a")
expect !compare_with("b", "a")

expect {
	expected = "a"
	Expr := [Leaf(Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left), Leaf(right)) => left == expected and right == expected
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf("a")) == Expr.Next(Expr.Leaf("a"))
}

expect {
	expected = "b"
	Expr := [Leaf(Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left), Leaf(right)) => left == expected and right == expected
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf("a")) != Expr.Next(Expr.Leaf("a"))
}
