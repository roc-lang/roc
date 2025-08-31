# META
~~~ini
description=record_literal_field_bang
type=expr
~~~
# SOURCE
~~~roc
{
    answer: 42,
    launchTheNukes!: |{}| ...,
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpBang OpColon OpBar OpenCurly CloseCurly OpBar TripleDot Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "answer")
    (num_literal_i32 42)
  )
  (binop_colon
    (not_lc "launchTheNukes")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (record_literal)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
{ answer : 42, launchTheNukes! : |{}| ... }
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_literal_field_bang.md:2:5:2:15:**
```roc
    answer: 42,
```
    ^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_literal_field_bang.md:3:5:3:30:**
```roc
    launchTheNukes!: |{}| ...,
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
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
