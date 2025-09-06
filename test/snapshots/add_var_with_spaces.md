# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

add2 = x +      2
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "add2")
))
~~~
# FORMATTED
~~~roc
module [add2]

add2 = x + 2
~~~
# EXPECTED
UNDEFINED VARIABLE - add_var_with_spaces.md:3:8:3:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**add_var_with_spaces.md:3:8:3:9:**
```roc
add2 = x +      2
```
       ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "add2"))
    (Expr.binop_plus
      (Expr.lookup "x")
      (Expr.num_literal_i32 2)
    )
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
