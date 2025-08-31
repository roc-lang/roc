# META
~~~ini
description=Record with single field
type=expr
~~~
# SOURCE
~~~roc
{ name: "test" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "name")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
name : "test"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**test_record_one_field.md:1:3:1:7:**
```roc
{ name: "test" }
```
  ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_small)
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
