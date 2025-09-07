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
(block
  (binop_equals
    (lc "foo")
    (if_without_else
      (condition         (lc "tru")
)
      (then         (num_literal_i32 0)
))
  )
)
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


**SHADOWING**
This definition shadows an existing one.

**expr_if_missing_else.md:3:1:3:4:**
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
; Total type variables: 7
(var #0 _)
(var #1 -> #4)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
foo : _a
~~~
