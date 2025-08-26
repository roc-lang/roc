# META
~~~ini
description=Single type argument application in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two"])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare String Comma String CloseSquare CloseRound ~~~
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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processList")
          (list_literal
            (str_literal_small "one")
            (str_literal_small "two")
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

processList : List Str -> U64
processList = \list -> list.len()
main! = \_ -> processList(["one", "two"])
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
