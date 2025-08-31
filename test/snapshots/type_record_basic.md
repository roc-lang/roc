# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({name: "luke", age:21})
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
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

getName : {name : Str, age : U64} -> Str
getName = |_person| "hello"

main! = |_| getName({ name : "luke", age : 21 })
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_record_basic.md:6:22:6:26:**
```roc
main! = |_| getName({name: "luke", age:21})
```
                     ^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_record_basic.md:6:36:6:39:**
```roc
main! = |_| getName({name: "luke", age:21})
```
                                   ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "getName")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "getName"))
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
