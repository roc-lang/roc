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
**UNUSED VARIABLE**
Variable **_dict** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__dict` to suppress this warning.
The unused variable is declared here:

**type_app_multiple_args.md:4:16:4:21:**
```roc
processDict = |_dict| []
```
               ^^^^^


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
