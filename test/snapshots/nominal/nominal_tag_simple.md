# META
~~~ini
description=Example of a simple nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Color")

    (lc "blue")
))
~~~
# FORMATTED
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_simple.md:3:7:3:9:**
```roc
Color := [Red, Green, Blue]
```
      ^^


**UNDEFINED VARIABLE**
Nothing is named **Color.Blue** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_simple.md:6:8:6:18:**
```roc
blue = Color.Blue
```
       ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Color.Yellow** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_simple.md:9:10:9:22:**
```roc
yellow = Color.Yellow
```
         ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.type_anno
    (name "blue")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "blue"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.type_anno
    (name "yellow")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "yellow"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
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
