RecursiveEqualityLocal := {}

compare_with = |expected, value| {
	Expr := [Leaf(Str, Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left, wanted), Leaf(right, _)) => left == wanted and right == wanted
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf(value, expected)) == Expr.Next(Expr.Leaf(value, expected))
}

expect compare_with("a", "a")
expect !compare_with("b", "a")

expect {
	expected = "a"
	Expr := [Leaf(Str, Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left, wanted), Leaf(right, _)) => left == wanted and right == wanted
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf("a", expected)) == Expr.Next(Expr.Leaf("a", expected))
}

expect {
	expected = "b"
	Expr := [Leaf(Str, Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left, wanted), Leaf(right, _)) => left == wanted and right == wanted
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
	Expr.Next(Expr.Leaf("a", expected)) != Expr.Next(Expr.Leaf("a", expected))
}
