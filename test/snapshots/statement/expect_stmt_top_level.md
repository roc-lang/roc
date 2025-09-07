# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = Bool.True

expect foo != Bool.False
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine KwExpect LowerIdent OpNotEquals UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
(block
  (binop_equals
    (lc "foo")
    (binop_dot
      (uc "Bool")
      (uc "True")
    )
  )
  (expect
    (binop_not_equals
      (lc "foo")
      (binop_dot
        (uc "Bool")
        (uc "False")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = (Bool.True)
expect foo != (Bool.False)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 -> #14)
(var #3 _)
(var #4 _)
(var #5 -> #14)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~
