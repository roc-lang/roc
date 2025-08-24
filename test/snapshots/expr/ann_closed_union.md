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
OpenCurly LowerIdent OpColon OpenSquare UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound CloseSquare LowerIdent OpAssign UpperIdent LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "apple")
    (list_literal
      (tuple_literal
        (uc "Apple")
        (apply_uc
          (uc "IsFruit")
          (uc "Bool")
        )
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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:10 to 3:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "apple")
    (Expr.malformed)
  )
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
