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
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**test_record_one_field.md:1:3:1:15:**
```roc
{ name: "test" }
```
  ^^^^^^^^^^^^


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
