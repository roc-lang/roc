# META
~~~ini
description=Simple qualified tag test
type=file
~~~
# SOURCE
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Color")
))
~~~
# FORMATTED
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_tag.md:3:7:3:9:**
```roc
Color := [Red, Blue]
```
      ^^


**UNDEFINED VARIABLE**
Nothing is named **Color.Red** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_tag.md:5:8:5:17:**
```roc
test = Color.Red
```
       ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "test"))
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
