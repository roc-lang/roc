# META
~~~ini
description=Return statement outside of function should be a compile error
type=repl
~~~
# SOURCE
~~~roc
» return 42
~~~
# OUTPUT
**RETURN OUTSIDE FUNCTION**
The **return** keyword can only be used inside a function body.

Use `return` to exit early from a function and provide a value. For example:
    foo = |x| { if x < 0 { return Err(NegativeInput) }; Ok(x) }
# PROBLEMS
NIL
