# META
~~~ini
description=Two distinct type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "swap")
    (binop_thin_arrow
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "b")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "swap")
    (lambda
      (body
        (tuple_literal
          (lc "y")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

swap :
	(a, b) -> (b, a)
swap = |x, y| (y, x)
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:10 to 4:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "swap")
    (Expr.binop_thin_arrow
      (Expr.tuple_literal
        (Expr.lookup "a")
        (Expr.lookup "b")
      )
      (Expr.tuple_literal
        (Expr.lookup "b")
        (Expr.lookup "a")
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "swap")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
swap : _c
~~~
