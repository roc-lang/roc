RecursiveEquality :: [].{
	Expr := [Leaf(Str), Next(Expr)].{
		is_eq = |self, other|
			match (self, other) {
				(Leaf(left), Leaf(right)) => left == right
				(Next(left), Next(right)) => left == right
				_ => False
			}
	}
}

make : Str -> Expr
make = |value| Next(Leaf(value))

expect Next(Leaf("a")) == make("a")
expect Expr.Next(Expr.Leaf("a")) == make("a")
expect make("a") == Next(Leaf("a"))
expect Expr.Next(Expr.Leaf("b")) != make("a")
expect Next(Leaf("a")).is_eq(make("a"))
expect Expr.Next(Expr.Leaf("a")).is_eq(make("a"))

# Keep equality generic until its callers supply the recursive type.
same = |left, right| left == right
expect same(make("a"), make("a"))

# Custom equality must survive both the direct and structural entry paths.
Lenient := [Leaf(Str), Next(Lenient)].{
	is_eq = |self, other|
		match (self, other) {
			(Leaf(_), Leaf(_)) => True
			(Next(left), Next(right)) => left == right
			_ => False
		}
}

lenient : Str -> Lenient
lenient = |value| Next(Leaf(value))

expect Next(Leaf("a")) == lenient("b")
expect Lenient.Next(Lenient.Leaf("a")) == lenient("b")
expect (Lenient.Next(Lenient.Leaf("a")) != lenient("b")).not()

NeverEqual := [Leaf(Str), Next(NeverEqual)].{
	is_eq = |self, other|
		match (self, other) {
			(Leaf(left), Leaf(right)) => left == right and False
			(Next(left), Next(right)) => left == right and False
			_ => False
		}
}

never_equal : Str -> NeverEqual
never_equal = |value| Leaf(value)

expect (Leaf("a") == never_equal("a")).not()
expect Leaf("a") != never_equal("a")

# Eager dot-call publication must also work without recursive requirements.
Key := [Val(Str)].{
	is_eq = |Val(left), Val(right)| left == right
}

key : Str -> Key
key = |value| Val(value)

expect Val("a").is_eq(key("a"))
expect Val("b").is_eq(key("a")).not()
