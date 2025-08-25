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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Str")
~~~
# TYPES
~~~roc
hello : Str
world : Str
~~~
