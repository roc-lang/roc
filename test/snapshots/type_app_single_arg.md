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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare String Comma String CloseSquare CloseRound ~~~
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

processList : List Str -> U64
processList = |list| list.len()

main! = |_| processList(["one", "two"])
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**type_app_single_arg.md:4:22:4:26:**
```roc
processList = |list| list.len()
```
                     ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "processList")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processList"))
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
