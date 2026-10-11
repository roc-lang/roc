# Two callable values, each built by a block, that are mutually recursive.
# Each producer is a block whose lambda captures `z` and refers to the other
# value, so a lambda lowered under one value's recursive binding names that
# binding and must not be shared with a copy lowered where it is not in scope.
MixedValueRecursion :: [].{}

is_even : U64 -> Bool
is_even = {
	z = 0
	|n| if n == z { Bool.True } else { match is_odd(n - 1) { Yes => Bool.True, No => Bool.False } }
}

is_odd : U64 -> [Yes, No]
is_odd = {
	z = 0
	|n| if n == z { No } else { if is_even(n - 1) { Yes } else { No } }
}

expect is_even(4)
expect !is_even(3)
expect is_odd(3) == Yes
expect is_odd(4) == No
