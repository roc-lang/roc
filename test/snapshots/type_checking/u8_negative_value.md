# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = -1
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "x")
    (uc "U8")
  )
  (binop_equals
    (lc "x")
    (unary_neg <unary_op>)
  )
)
~~~
# FORMATTED
~~~roc
module []

x : U8
x = -1
~~~
# EXPECTED
NEGATIVE UNSIGNED INTEGER - u8_negative_value.md:4:5:4:7
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**u8_negative_value.md:3:1:3:2:**
```roc
x : U8
```
^


**SHADOWING**
This definition shadows an existing one.

**u8_negative_value.md:4:1:4:2:**
```roc
x = -1
```
^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "x"))
    (type type_2)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.unary_neg)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 Num *)
(var #6 -> #5)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
x : Num(_size)
~~~
