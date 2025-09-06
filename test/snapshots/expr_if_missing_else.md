# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = if tru 0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign KwIf LowerIdent Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

foo = if tru 0
~~~
# EXPECTED
IF WITHOUT ELSE - expr_if_missing_else.md:3:7:3:9
UNRECOGNIZED SYNTAX - expr_if_missing_else.md:3:7:3:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **tru** in this scope.
Is there an **import** or **exposing** missing up-top?

**expr_if_missing_else.md:3:10:3:13:**
```roc
foo = if tru 0
```
         ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.if_else)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
