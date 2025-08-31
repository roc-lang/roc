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
**UNDEFINED VARIABLE**
Nothing is named **answer** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_literal_field_bang.md:2:5:2:11:**
```roc
    answer: 42,
```
    ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **launchTheNukes!** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_literal_field_bang.md:3:5:3:20:**
```roc
    launchTheNukes!: |{}| ...,
```
    ^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "answer")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.lambda (canonicalized))
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
