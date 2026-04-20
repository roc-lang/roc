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
The `return` keyword can only be used inside function bodies.

**repl:2:5:2:14:**
```roc
    return 42
```
    ^^^^^^^^^
# PROBLEMS
NIL
