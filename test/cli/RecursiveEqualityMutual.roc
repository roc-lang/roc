RecursiveEqualityMutual := {}

Expr := [Leaf(Str), Link(Tail)].{
	is_eq = |self, other|
		match (self, other) {
			(Leaf(left), Leaf(right)) => left == right
			(Link(left), Link(right)) => left == right
			_ => False
		}
}

Tail := [Back(Expr)].{
	is_eq = |Back(left), Back(right)| left == right
}

make : Str -> Expr
make = |value| Link(Back(Leaf(value)))

expect Expr.Link(Tail.Back(Expr.Leaf("a"))) == make("a")
expect make("a") != make("b")
expect Link(Back(Leaf("a"))).is_eq(make("a"))
