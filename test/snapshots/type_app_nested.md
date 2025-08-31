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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare String Comma String CloseSquare BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processNested : List Result(Str, Err) -> List Str
processNested = |_list| ["one", "two"]

main! = |_| processNested([])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "processNested")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processNested"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
