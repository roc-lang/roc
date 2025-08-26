# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = Bool.True

expect foo != Bool.False
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign UpperIdent Dot UpperIdent KwExpect LowerIdent OpNotEquals UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (binop_pipe
      (uc "Bool")
      (uc "True")
    )
  )
  (expect
    (binop_not_equals
      (lc "foo")
      (binop_pipe
        (uc "Bool")
        (uc "False")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	foo,
]

foo = Bool.True
expect foo != Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_minus)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
