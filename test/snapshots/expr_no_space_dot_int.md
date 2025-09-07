# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign LowerIdent Dot Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "foo")
    (binop_dot
      (lc "asd")
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

foo = (asd.0)
~~~
# EXPECTED
PARSE ERROR - expr_no_space_dot_int.md:3:10:3:12
UNRECOGNIZED SYNTAX - expr_no_space_dot_int.md:3:10:3:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **asd** in this scope.
Is there an **import** or **exposing** missing up-top?

**expr_no_space_dot_int.md:3:7:3:10:**
```roc
foo = asd.0
```
      ^^^


**SHADOWING**
This definition shadows an existing one.

**expr_no_space_dot_int.md:3:1:3:4:**
```roc
foo = asd.0
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.record_access)
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
