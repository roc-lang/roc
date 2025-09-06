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
(block
  (binop_equals
    (lc "add2")
    (binop_plus
      (lc "x")
      (num_literal_i32 2)
    )
  )
)
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
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 -> #5)
(var #3 -> #4)
(var #4 -> #5)
(var #5 Num *)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
add2 : Num(_size)
~~~
