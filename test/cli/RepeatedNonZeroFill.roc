RepeatedNonZeroFill := [].{
	ones : U64 -> List(U32)
	ones = |n| List.repeat(1.U32, n)
}

# A list filled with a repeated non-zero value, which optimized code on Apple
# targets builds with a call to memset_pattern16.

expect RepeatedNonZeroFill.ones(30) == List.repeat(1.U32, 30)

expect RepeatedNonZeroFill.ones(30).sum() == 30
