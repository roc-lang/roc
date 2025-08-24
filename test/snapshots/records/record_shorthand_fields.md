# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (lc "name")
  (tuple_literal
    (lc "age")
    (lc "email")
    (lc "active")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - record_shorthand_fields.md:1:3:1:7
UNDEFINED VARIABLE - record_shorthand_fields.md:1:9:1:12
UNDEFINED VARIABLE - record_shorthand_fields.md:1:14:1:19
UNDEFINED VARIABLE - record_shorthand_fields.md:1:21:1:27
# PROBLEMS
**Unsupported Node**
at 1:28 to 1:28

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "name")
  (Expr.malformed)
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
