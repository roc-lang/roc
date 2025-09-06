# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]0 f
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Int LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (num_literal_i32 0)
  (lc "f")
)
~~~
# FORMATTED
~~~roc
module []

0
f
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_034.md:1:9:1:10
PARSE ERROR - fuzz_crash_034.md:1:11:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **f** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_034.md:1:11:1:12:**
```roc
module[]0 f
```
          ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.lookup "f")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 Num *)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
