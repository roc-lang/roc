# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
{
	hello = "Hello"
	world = "World"
	"${hello} ${world}"
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign String LowerIdent OpAssign String String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "hello")
    (str_literal_big "Hello")
  )
  (binop_equals
    (lc "world")
    (str_literal_big "World")
  )
  (str_literal_big "${hello} ${world}")
)
~~~
# FORMATTED
~~~roc
hello = "Hello"
world = "World"
"${hello} ${world}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "hello"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "world"))
    (Expr.str_literal_big)
  )
  (Expr.str_literal_big)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 -> #2)
(var #2 Str)
(var #3 _)
(var #4 -> #5)
(var #5 Str)
(var #6 _)
(var #7 Str)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
