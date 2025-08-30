# META
~~~ini
description=Test comprehensive comment preservation
type=file
~~~
# SOURCE
~~~roc
module [
    # First comment
    foo, # inline comment after foo
    bar, # inline comment after bar
]

# Comment before function
foo = 
    # Comment in function body
    42 # inline comment after value

# Comment between functions

bar = # comment after equals
    100

# Trailing comment at end of file
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare LowerIdent OpAssign Int LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")

    (lc "bar")
))
~~~
# FORMATTED
~~~roc
module [
	foo,
	bar,
]

foo = 42
bar = 100
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "bar")
    (Expr.num_literal_i32 100)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : Num(_size)
bar : Num(_size)
~~~
