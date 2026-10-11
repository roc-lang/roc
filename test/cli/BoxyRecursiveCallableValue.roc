# Recursive top-level callable values whose producers are blocks. Each
# compile-time evaluated closure captures its block local and the recursive
# binding of the value it refers back to; a Boxy worker reaches that value
# through its own top-level reference instead of a capture slot.
BoxyRecursiveCallableValue :: [].{}

count : U64 -> U64
count = {
	z = 0
	|n| if n == z { z } else { count(n - 1) + 1 }
}

expect count(4) == 4
expect count(0) == 0

# Mutual recursion between two block-built values.
is_even : U64 -> Bool
is_even = {
	z = 0
	|n| if n == z { Bool.True } else { is_odd(n - 1) }
}

is_odd : U64 -> Bool
is_odd = {
	z = 0
	|n| if n == z { Bool.False } else { is_even(n - 1) }
}

expect is_even(4)
expect is_odd(3)
expect !is_odd(4)

# Three-way recursion.
step_a : U64 -> U64
step_a = {
	z = 0
	|n| if n == z { 0 } else { step_b(n - 1) + 1 }
}

step_b : U64 -> U64
step_b = {
	z = 0
	|n| if n == z { 0 } else { step_c(n - 1) + 10 }
}

step_c : U64 -> U64
step_c = {
	z = 0
	|n| if n == z { 0 } else { step_a(n - 1) + 100 }
}

expect step_a(3) == 111
expect step_c(4) == 211

# Recursion through a lambda nested two levels inside the value's lambda.
nested_count : U64 -> U64
nested_count = {
	z = 0
	|n| {
		inner = |m| {
			deeper = |k| if k == z { z } else { nested_count(k - 1) + 1 }
			deeper(m)
		}
		inner(n)
	}
}

expect nested_count(4) == 4
