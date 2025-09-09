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
    (Expr.fn_call)
  )
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #9)
(var #5 _)
(var #6 -> #9)
(var #7 -> #14)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 -> #8)
(var #14 -> #15)
(var #15 fn_pure)
~~~
# TYPES
~~~roc
~~~
