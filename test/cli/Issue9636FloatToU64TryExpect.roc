Issue9636FloatToU64TryExpect :: [].{}

expect {
	is_approx_eq(0.999995.F64, 1.00005.F64)
}

is_approx_eq : F64, F64 -> Bool
is_approx_eq = |x1, x2| {
	i1 = (x1 * 1000 + 0.5).to_u64_try() ?? {
		crash "Unreachable"
	}
	i2 = (x2 * 1000 + 0.5).to_u64_try() ?? {
		crash "Unreachable"
	}
	i1 == i2
}
