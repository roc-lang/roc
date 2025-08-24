# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = |num| {
    # statement - prints out the value of num convertert to a string
    dbg num.to_str()

    # expression - prints out the value of num and then returns it
    dbg(num)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwDbg LowerIdent Dot LowerIdent OpenRound CloseRound KwDbg OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (lambda
      (body
        (block
          (malformed malformed:expr_unexpected_token)
          (apply_anon
            (binop_pipe
              (lc "num")
              (dot_lc "to_str")
            )
          )
          (apply_anon
            (malformed malformed:expr_unexpected_token)
            (lc "num")
          )
        )
      )
      (args
        (lc "num")
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
**Parse Error**
at 5:5 to 5:5

**Parse Error**
at 8:5 to 8:5

**Unsupported Node**
at 3:7 to 3:13

# CANONICALIZE
~~~clojure
(Expr.block
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
