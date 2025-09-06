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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "apple"))
    (type type_6)
  )
  (Stmt.assign
    (pattern (Patt.ident "apple"))
    (Expr.tag_no_args)
  )
  (Expr.lookup "apple")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #9)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
apple : _a
~~~
