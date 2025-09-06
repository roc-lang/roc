# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# TOKENS
~~~text
LowerIdent Dot Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_dot
    (binop_dot
      (lc "ff8")
      (num_literal_i32 8)
    )
    (dot_lc "d")
  )
)
~~~
# FORMATTED
~~~roc
ff8.8.d..d
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_007.md:1:1:1:4
PARSE ERROR - fuzz_crash_007.md:1:4:1:6
PARSE ERROR - fuzz_crash_007.md:1:6:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **ff8** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_007.md:1:1:1:4:**
```roc
ff8.8.d
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.record_access)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
~~~
