# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# TOKENS
~~~text
UpperIdent OpenCurly LowerIdent Comma CloseSquare LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (uc "H")
  (record_literal
    (lc "o")
    (malformed malformed:expr_unexpected_token)
  )
  (binop_equals
    (lc "foo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
H{ o, <malformed> }foo = <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:6 to 2:6

**Parse Error**
at 1:2 to 3:1

**Parse Error**
at 5:5 to 5:5

**Unsupported Node**
at 2:6 to 2:6

**Unsupported Node**
at 5:5 to 5:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
  (Expr.record_literal
    (Expr.lookup "o")
    (Expr.malformed)
  )
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
