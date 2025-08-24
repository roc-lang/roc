# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpAssign String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "age")
    (binop_equals
      (tuple_literal
        (num_literal_i32 42)
        (lc "name")
      )
      (str_literal_big "Alice")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED TOKEN IN EXPRESSION - error_malformed_syntax_2.md:1:10:1:11
MALFORMED TYPE - error_malformed_syntax_2.md:1:8:1:10
UNUSED VARIABLE - error_malformed_syntax_2.md:1:12:1:16
# PROBLEMS
**Unsupported Node**
at 1:17 to 1:17

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.binop_equals
      (Expr.malformed)
      (Expr.str_literal_big)
    )
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
