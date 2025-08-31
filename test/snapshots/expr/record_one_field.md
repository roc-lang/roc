# META
~~~ini
description=Single field record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
)
~~~
# FORMATTED
~~~roc
name : "Alice"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_one_field.md:1:3:1:7:**
```roc
{ name: "Alice" }
```
  ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_big)
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
