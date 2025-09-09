# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign Float LowerIdent OpAssign Float LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "x")
    (frac_literal_small 3.14)
  )
  (binop_equals
    (lc "y")
    (frac_literal_big frac:<idx:8>)
  )
  (binop_equals
    (lc "z")
    (frac_literal_small 0.5)
  )
)
~~~
# FORMATTED
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**can_frac_literal.md:3:1:3:2:**
```roc
x = 3.14
```
^


**SHADOWING**
This definition shadows an existing one.

**can_frac_literal.md:4:1:4:2:**
```roc
y = 1.23e45
```
^


**SHADOWING**
This definition shadows an existing one.

**can_frac_literal.md:5:1:5:2:**
```roc
z = 0.5
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.frac_literal_small 3.14)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.frac_literal_big big:<idx:8>)
  )
  (Stmt.assign
    (pattern (Patt.ident "z"))
    (Expr.frac_literal_small 0.5)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 -> #2)
(var #2 F64)
(var #3 _)
(var #4 -> #5)
(var #5 F64)
(var #6 _)
(var #7 -> #8)
(var #8 F64)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
x : F64
y : F64
z : F64
~~~
