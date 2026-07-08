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
**Type Mismatch**
The first argument being passed to this function has the wrong type.
**repl:3:16:3:17:**
```roc
b = Dec.to_str(x)
```
               ^

This argument has the type:

    I64

But the function needs the first argument to be:

    Dec
---
**Name Not In Scope**
Nothing is named `b` in this scope.
Is it misspelled, or is there an import missing?

**repl:3:22:3:23:**
```roc
main = Str.concat(a, b)
```
                     ^
# PROBLEMS
NIL
