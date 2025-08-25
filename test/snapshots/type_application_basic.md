# META
~~~ini
description=Basic type application canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two","three"])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare String Comma String Comma String CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "processList")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (uc "Str")
      )
      (uc "U64")
    )
  )
  (binop_equals
    (lc "processList")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "list")
            (dot_lc "len")
          )
        )
      )
      (args
        (lc "list")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (apply_lc
      (lc "processList")
      (list_literal
        (tuple_literal
          (str_literal_small "one")
          (str_literal_small "two")
          (str_literal_big "three")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/main.roc" platform [main]) }

processList: (List(Str) -> U64)
processList = \list -> list | .len()
main
(<malformed>! | _) | processList([("one", "two", "three")])
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:15 to 3:31

**Unsupported Node**
at 4:15 to 4:22

**Unsupported Node**
at 6:5 to 6:7

**Unsupported Node**
at 6:25 to 6:46

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "processList")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> _ret")
~~~
# TYPES
~~~roc
~~~
