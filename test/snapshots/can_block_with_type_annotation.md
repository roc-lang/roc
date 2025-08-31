# META
~~~ini
description=Block with type annotation and assignment
type=expr
~~~
# SOURCE
~~~roc
{ x : Str
  x = "hello" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "x")
    (uc "Str")
  )
  (binop_equals
    (lc "x")
    (str_literal_big "hello")
  )
)
~~~
# FORMATTED
~~~roc
x : Str
x = "hello"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**can_block_with_type_annotation.md:2:3:2:4:**
```roc
  x = "hello" }
```
  ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "x")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Str
~~~
