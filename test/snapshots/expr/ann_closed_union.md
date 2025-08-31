# META
~~~ini
description=Closed Tag Union Type Annotation
type=expr
~~~
# SOURCE
~~~roc
{
	apple : [Apple, IsFruit(Bool)]
	apple = Apple

	apple
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenSquare UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare LowerIdent OpAssign UpperIdent BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "apple")
    (list_literal
      (uc "Apple")
      (apply_uc
        (uc "IsFruit")
        (uc "Bool")
      )
    )
  )
  (binop_equals
    (lc "apple")
    (uc "Apple")
  )
  (lc "apple")
)
~~~
# FORMATTED
~~~roc
apple : [Apple, IsFruit(Bool)]
apple = Apple
apple
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
  (Expr.lookup "apple")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
apple : []_others
~~~
