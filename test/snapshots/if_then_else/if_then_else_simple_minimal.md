# META
~~~ini
description=if_then_else (1)
type=expr
~~~
# SOURCE
~~~roc
if bool 1 else 2
~~~
# TOKENS
~~~text
KwIf LowerIdent Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else
  (condition     (lc "bool")
)
  (then     (num_literal_i32 1)
)
  (else     (num_literal_i32 2)
))
~~~
# FORMATTED
~~~roc
if bool 1 else 2
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_simple_minimal.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **bool** in this scope.
Is there an **import** or **exposing** missing up-top?

**if_then_else_simple_minimal.md:1:4:1:8:**
```roc
if bool 1 else 2
```
   ^^^^


# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
