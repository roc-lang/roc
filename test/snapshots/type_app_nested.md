# META
~~~ini
description=Nested type applications in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one","two"]

main! = |_| processNested([])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare String Comma String CloseSquare LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "processNested")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (uc "Str")
            (uc "Err")
          )
        )
      )
      (apply_uc
        (uc "List")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "processNested")
    (lambda
      (body
        (list_literal
          (str_literal_small "one")
          (str_literal_small "two")
        )
      )
      (args
        (lc "_list")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processNested")
          (list_literal)
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

processNested : List Result (Str, Err) -> List Str
processNested = \_list -> ["one", "two"]

main! = \_ -> processNested([])
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
