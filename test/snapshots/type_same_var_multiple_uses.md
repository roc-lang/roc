# META
~~~ini
description=Multiple uses of same type variable in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

pair : a -> (a, a)
pair = |x| (x, x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "pair")
    (binop_thin_arrow
      (lc "a")
      (tuple_literal
        (lc "a")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "pair")
    (lambda
      (body
        (tuple_literal
          (lc "x")
          (lc "x")
        )
      )
      (args
        (lc "x")
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

pair : a -> (a, a)
pair = |x| (x, x)
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "pair")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.tuple_literal
        (Expr.lookup "a")
        (Expr.lookup "a")
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "pair")
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
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
pair : _b
~~~
