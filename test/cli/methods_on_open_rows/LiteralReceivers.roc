# `==` relates both operands to one type, so a literal compared with a named
# value takes the named type. An explicit method call has no such rule: a
# literal receiver is plain, so `.is_eq(...)` and `.foo(...)` on it are
# rejected when the argument is a named type.

LiteralReceivers :: [].{}

Expr := [Leaf(Str), Next(Expr)].{
	is_eq : Expr, Expr -> Bool
	is_eq = |_, _| Bool.True

	foo : Expr, Expr -> Bool
	foo = |_, _| Bool.True
}

Tagged := { id : U64 }.{
	is_eq : Tagged, Tagged -> Bool
	is_eq = |_, _| Bool.True

	foo : Tagged, Tagged -> Bool
	foo = |_, _| Bool.True
}

e : Expr
e = Leaf("b")

one : Tagged
one = { id: 2 }

expect Leaf("a") == e
expect { id: 1 } == one
expect Leaf("a").is_eq(e)
expect Leaf("a").foo(e)
expect { id: 1 }.is_eq(one)
expect { id: 1 }.foo(one)
