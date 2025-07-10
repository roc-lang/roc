# Test file that uses a not-implemented feature
x = 42

# This should trigger a "not implemented" error
when x is
    42 -> "correct"
    _ -> "wrong"
