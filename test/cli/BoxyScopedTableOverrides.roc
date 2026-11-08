BoxyScopedTableOverrides :: [].{}

# An open nominal whose `is_eq` override requires a method of its type
# parameter, compared structurally in a generic body: the descriptor the
# body's table holds gives the override the body's own dictionary for that
# parameter, which no static descriptor could.
Loose := [L(U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |a, b|
		match (a, b) {
			(L(x), L(y)) => x % 10 == y % 10
		}
}

Pair(a) := { v : a }.{
	is_eq : Pair(a), Pair(a) -> Bool where [a.is_eq : a, a -> Bool]
	is_eq = |x, y| x.v == y.v
}

wrap : a -> Pair(a)
wrap = |value| { v: value }

same_boxed : a, a -> Bool where [a.is_eq : a, a -> Bool]
same_boxed = |x, y| { p: wrap(x) } == { p: wrap(y) }

expect same_boxed(Loose.L(1), Loose.L(11))
expect !same_boxed(Loose.L(1), Loose.L(2))
expect same_boxed(1.U64, 1.U64)
expect !same_boxed(1.U64, 11.U64)

# A descriptor whose override only the declaring body can build, passed to
# a generic function's table, and from that table into the table of the
# function it calls.
show_twice : t -> Str
show_twice = |v| Str.concat(Str.inspect(v), Str.inspect([v]))

local_through_tables : a -> Str
local_through_tables = |x| {
	Local := [Local(a)].{
		to_inspect = |_value| "custom"
	}
	show_twice(Local.Local(x))
}

expect local_through_tables(5.U64) == "custom[custom]"
