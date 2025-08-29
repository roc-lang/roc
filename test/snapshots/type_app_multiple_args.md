# META
~~~ini
description=Multiple type arguments application in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processDict : Dict(Str, U64) -> List(Str)
processDict = |_dict| []

main! = |_| processDict(Dict.empty().insert("one", 1))
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare CloseSquare LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent Dot LowerIdent OpenRound CloseRound Dot LowerIdent OpenRound String Comma Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "processDict")
    (binop_thin_arrow
      (apply_uc
        (uc "Dict")
        (tuple_literal
          (uc "Str")
          (uc "U64")
        )
      )
      (apply_uc
        (uc "List")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "processDict")
    (lambda
      (body
        (list_literal)
      )
      (args
        (lc "_dict")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processDict")
          (apply_anon
            (binop_pipe
              (apply_anon
                (binop_pipe
                  (uc "Dict")
                  (dot_lc "empty")
                )
              )
              (dot_lc "insert")
            )
            (tuple_literal
              (str_literal_small "one")
              (num_literal_i32 1)
            )
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
app { pf: "../basic-cli/main.roc" platform [main] }

processDict : Dict(Str, U64) -> List Str
processDict = |_dict| []
main! = |_| processDict(Dict.empty() | .insert(("one", 1)))
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 6:25 to 6:37

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "processDict")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processDict")
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
processDict : _a
~~~
