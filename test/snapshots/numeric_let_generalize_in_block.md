# META
~~~ini
description=Numeric let-generalization inside nested block (rank > top_level)
type=expr
~~~

# NOTES
This test demonstrates that numeric literals inside nested blocks (rank > top_level)
ARE let-generalized, allowing the same numeric variable to be used with different
concrete numeric types within the block.

This is different from top-level behavior where numeric literals stay monomorphic
so that later usages can constrain them to a specific type.

The key insight is that rank > top_level can occur in two situations:
1. Inside lambdas (e.g., `|a| a + 1`)
2. Inside nested blocks (e.g., `{ n = 42; ... }`)

In both cases, numeric literals are generalized.

# SOURCE
~~~roc
{
    n = 42
    a = I64.to_str(n)
    b = Dec.to_str(n)
    Str.concat(a, b)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
