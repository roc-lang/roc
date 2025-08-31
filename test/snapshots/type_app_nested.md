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
**UNUSED VARIABLE**
Variable **_list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__list` to suppress this warning.
The unused variable is declared here:

**type_app_nested.md:4:18:4:23:**
```roc
processNested = |_list| ["one","two"]
```
                 ^^^^^


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
