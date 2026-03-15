# META
~~~ini
description=Numeric without annotation, later use gives type error (no let-generalization for non-lambdas)
type=repl
~~~
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
**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**repl:4:20:4:21:**
```roc
    b = Dec.to_str(x)
```
                   ^

This argument has the type:

    I64

But the function needs the first argument to be:

    Dec
# PROBLEMS
NIL
