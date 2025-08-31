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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_one_field.md:1:3:1:16:**
```roc
{ name: "Alice" }
```
  ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
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
