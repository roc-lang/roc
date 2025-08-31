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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**can_single_field_record.md:1:3:1:7:**
```roc
{ x: 1 }
```
  ^^^^


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
