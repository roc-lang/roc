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
**Return Outside Function**
The `return` keyword can only be used inside function bodies.
```roc
    return 42
```
    ^^^^^^^^^
# PROBLEMS
NIL
