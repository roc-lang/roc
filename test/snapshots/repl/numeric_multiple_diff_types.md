# META
~~~ini
description=Numeric without annotation, multiple uses with different types (each use gets fresh type)
type=repl
~~~

# NOTES
This test demonstrates that in the REPL, a numeric literal assigned without
annotation can be used with different concrete types in subsequent statements.

Each use of `x` gets a fresh instantiation of the type, allowing it to be
constrained to I64 in one statement and Dec in another.

This is the expected behavior for polymorphic numeric literals - each use
site gets its own copy of the type variable that can be independently constrained.

Compare this to the non-REPL test `numeric_let_generalize_in_block.md` which
demonstrates the same polymorphic behavior inside nested blocks.

# SOURCE
~~~roc
» x = 42
» a = I64.to_str(x)
» b = Dec.to_str(x)
» Str.concat(a, b)
~~~
# OUTPUT
assigned `x`
---
assigned `a`
---
assigned `b`
---
"4242.0"
# PROBLEMS
NIL
