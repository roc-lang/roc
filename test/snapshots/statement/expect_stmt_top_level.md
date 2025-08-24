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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:7 to 3:11

**Unsupported Node**
at 5:1 to 5:24

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
