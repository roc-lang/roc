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
UNUSED VARIABLE - block_pattern_unify.md:3:5:3:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "str"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.binop_plus
      (Expr.lookup "x")
      (Expr.num_literal_i32 5)
    )
  )
  (Expr.lookup "result")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #5)
(var #5 Str)
(var #6 _)
(var #7 -> #10)
(var #8 -> #9)
(var #9 -> #10)
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
x : Num(_size)
str : Str
result : Num(_size)
~~~
