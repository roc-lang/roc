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
KwModule OpenSquare LineComment LowerIdent Comma LineComment LowerIdent Comma LineComment CloseSquare BlankLine LineComment LowerIdent OpAssign LineComment Int LineComment BlankLine LineComment BlankLine LowerIdent OpAssign LineComment Int BlankLine LineComment ~~~
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
module [ # First comment
	foo,
	# inline comment after foo
	bar,
]

# inline comment after bar

# Comment before function
foo = # Comment in function body
42

# inline comment after value

# Comment between functions

bar = # comment after equals
100

# Trailing comment at end of file
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
