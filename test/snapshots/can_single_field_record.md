# META
~~~ini
description=Single field record
type=expr
~~~
# SOURCE
~~~roc
{ x: 1 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "x")
    (num_literal_i32 1)
  )
)
~~~
# FORMATTED
~~~roc
x : 1
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_single_field_record.md:1:3:1:4:**
```roc
{ x: 1 }
```
  ^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "x")
    (Expr.num_literal_i32 1)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
