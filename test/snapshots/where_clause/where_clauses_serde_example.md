# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "deserialize")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (apply_uc
            (uc "List")
            (uc "U8")
          )
          (apply_uc
            (uc "Result")
            (tuple_literal
              (lc "a")
              (list_literal
                (uc "DecodeErr")
              )
            )
          )
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "decode")
          )
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (list_literal
            (uc "DecodeErr")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "deserialize")
    (lambda
      (body
        (ellipsis)
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
module [deserialize]

deserialize : List U8 -> Result(a, [DecodeErr]) where module(a).decode : List U8 -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:14 to 4:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "deserialize")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "deserialize")
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
deserialize : _b
~~~
