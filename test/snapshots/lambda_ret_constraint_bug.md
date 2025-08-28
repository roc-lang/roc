# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app { pf: "platform/main.roc" platform [main] }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "helper")
    (binop_thin_arrow
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "helper")
    (lambda
      (body
        (binop_star
          (lc "n")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_colon
    (lc "main")
    (binop_thin_arrow
      (uc "I64")
      (binop_thin_arrow
        (uc "I64")
        (uc "I64")
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (apply_lc
          (lc "helper")
          (num_literal_i32 5)
        )
      )
      (args
        (tuple_literal
          (underscore)
          (underscore)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app
{
	pf: "platform/main.roc" platform [main],
}

helper : I64 -> I64
helper = \n -> n * 2
main : I64 -> I64 -> I64
main = \(_, _) -> helper(5)
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
