import RecursiveEquality exposing [Expr]

make : Str -> Expr
make = |value| Next(Leaf(value))

expect Next(Leaf("a")) == make("a")
expect Expr.Next(Expr.Leaf("a")) == make("a")
expect make("a") == Next(Leaf("a"))
expect Expr.Next(Expr.Leaf("b")) != make("a")
expect Next(Leaf("a")).is_eq(make("a"))
expect Expr.Next(Expr.Leaf("a")).is_eq(make("a"))
