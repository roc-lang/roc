# META
~~~ini
description=Hexadecimal integer literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 0xFF
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign IntBase ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "x")
    (int_literal_big int:<idx:4>)
  )
)
~~~
# FORMATTED
~~~roc
module []

x = 0xFF
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**can_hex_integer.md:3:1:3:2:**
```roc
x = 0xFF
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.int_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 -> #2)
(var #2 I128)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
x : I128
~~~
