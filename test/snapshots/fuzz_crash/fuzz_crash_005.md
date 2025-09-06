# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# TOKENS
~~~text
LowerIdent ~~~
# PARSE
~~~clojure
(block
  (lc "modu")
)
~~~
# FORMATTED
~~~roc
modu
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_005.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **modu** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_005.md:1:1:1:5:**
```roc
modu
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "modu")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 _)
(var #2 _)
~~~
# TYPES
~~~roc
~~~
