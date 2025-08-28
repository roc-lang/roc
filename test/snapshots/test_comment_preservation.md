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
(block
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "bar")
    (num_literal_i32 100)
  )
)
~~~
# FORMATTED
~~~roc
module [
	# First comment
	foo,
# inline comment after foo
	bar,
]

foo = # Comment in function body
42 # inline comment after value

	# Comment between functions

bar = # comment after equals
100
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
