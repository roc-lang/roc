# META
~~~ini
description=Block with pattern unification testing
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    str = "hello"
    result = x + 5
    result
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "str")
    (str_literal_big "hello")
  )
  (binop_equals
    (lc "result")
    (binop_plus
      (lc "x")
      (num_literal_i32 5)
    )
  )
  (lc "result")
)
~~~
# FORMATTED
~~~roc
x = 42
str = "hello"
result = x + 5
result
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
