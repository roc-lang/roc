# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
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
    (num_literal_i32 500)
  )
)
~~~
# FORMATTED
~~~roc
module []

x : U8
x = 500
~~~
# EXPECTED
NUMBER DOES NOT FIT IN TYPE - u8_annotation_large_value.md:4:5:4:8
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**u8_annotation_large_value.md:3:1:3:2:**
```roc
x : U8
```
^


**SHADOWING**
This definition shadows an existing one.

**u8_annotation_large_value.md:4:1:4:2:**
```roc
x = 500
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
    (Expr.num_literal_i32 500)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 Num *)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
x : Num(_size)
~~~
