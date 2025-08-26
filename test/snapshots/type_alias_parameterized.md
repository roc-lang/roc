# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (lc "swapPair")
    (binop_thin_arrow
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "b")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "swapPair")
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
        (apply_lc
          (lc "swapPair")
          (tuple_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
          )
        )
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
app
{
	pf: "../basic-cli/main.roc" platform [
		main,
	],
}

Pair((a, b)) : (a, b)

swapPair : Pair (a, b) -> Pair (b, a)
swapPair = \(x, y) -> (y, x)

main! = \_ -> swapPair((1, 2))
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
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
