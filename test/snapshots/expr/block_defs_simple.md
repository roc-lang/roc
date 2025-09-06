# META
~~~ini
description=Block expression with two decls and final binop expr
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    y = x + 1
    y * 2
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpStar Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "y")
    (binop_plus
      (lc "x")
      (num_literal_i32 1)
    )
  )
  (binop_star
    (lc "y")
    (num_literal_i32 2)
  )
)
~~~
# FORMATTED
~~~roc
x = 42
y = x + 1
y * 2
~~~
# EXPECTED
NIL
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
    (pattern (Patt.ident "y"))
    (Expr.binop_plus
      (Expr.lookup "x")
      (Expr.num_literal_i32 1)
    )
  )
  (Expr.binop_star
    (Expr.lookup "y")
    (Expr.num_literal_i32 2)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #7)
(var #5 -> #6)
(var #6 -> #7)
(var #7 Num *)
(var #8 _)
(var #9 -> #10)
(var #10 -> #11)
(var #11 Num *)
(var #12 _)
~~~
# TYPES
~~~roc
x : Num(_size)
y : Num(_size)
~~~
