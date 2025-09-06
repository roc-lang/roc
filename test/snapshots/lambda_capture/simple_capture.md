# META
~~~ini
description="A basic case where a lambda captures one variable from its immediate parent scope."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| x)(1)
    y
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenRound OpBar Underscore OpBar LowerIdent CloseRound OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "y")
    (apply_anon
      (lambda
        (body
          (lc "x")
        )
        (args
          (underscore)
        )
      )
      (num_literal_i32 1)
    )
  )
  (lc "y")
)
~~~
# FORMATTED
~~~roc
x = 5
y = (|_| x)(1)
y
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**simple_capture.md:3:15:3:16:**
```roc
    y = (|_| x)(1)
```
              ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.apply_ident)
  )
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
