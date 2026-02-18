module []

# Regression test: String literals of length >= 24 (big strings) must work
# correctly with equality comparisons. This tests the single-segment string
# fast path in str_collect.

# Exactly 24 characters - this is a "big string" (>= SMALL_STRING_SIZE)
expect ("123456789012345678901234" != "")
expect ("123456789012345678901234" == "123456789012345678901234")
expect ("" != "123456789012345678901234")

# 25 characters - definitely a big string
expect ("1234567890123456789012345" != "")
expect ("1234567890123456789012345" == "1234567890123456789012345")

# Compare two big strings
expect ("123456789012345678901234" != "123456789012345678901235")
