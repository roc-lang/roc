# META
~~~ini
description=Type application with variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1,2,3,4,5])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenSquare Int Comma Int Comma Int Comma Int Comma Int CloseSquare CloseRound ~~~
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

mapList : List a -> (a -> b) -> List b
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1, 2, 3, 4, 5])
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_app_with_vars.md:4:22:4:30:**
```roc
mapList = |list, fn| list.map(fn)
```
                     ^^^^^^^^


**UNUSED VARIABLE**
Variable **list** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:

**type_app_with_vars.md:4:12:4:16:**
```roc
mapList = |list, fn| list.map(fn)
```
           ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "mapList")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "mapList"))
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
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
