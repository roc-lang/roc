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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare CloseSquare BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent Dot LowerIdent OpenRound CloseRound Dot LowerIdent OpenRound String Comma Int CloseRound CloseRound ~~~
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

processDict : Dict(Str, U64) -> List Str
processDict = |_dict| []

main! = |_| processDict(Dict.empty() | .insert(("one", 1)))
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_app_multiple_args.md:6:25:6:37:**
```roc
main! = |_| processDict(Dict.empty().insert("one", 1))
```
                        ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "processDict")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processDict"))
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
