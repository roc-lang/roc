# META
~~~ini
description=Type shadowing across scopes should produce warning
type=file
~~~
# SOURCE
~~~roc
module [Result, processData]

Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String UpperIdent OpColon OpenCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Result")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (list_literal
      (apply_uc
        (uc "Ok")
        (lc "a")
      )
      (apply_uc
        (uc "Err")
        (lc "b")
      )
    )
  )
  (binop_colon
    (lc "processData")
    (binop_thin_arrow
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "processData")
    (lambda
      (body
        (str_literal_big "processed")
      )
      (args
        (lc "data")
      )
    )
  )
  (binop_colon
    (uc "InnerModule")
    (block
      (binop_colon
        (uc "Result")
        (list_literal
          (uc "Success")
          (uc "Failure")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Result, processData]

Result((a, b)) : [Ok(a), Err(b)]
processData : Str -> Str
processData = \data -> "processed"
InnerModule : {
	Result : [Success, Failure]
}
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
