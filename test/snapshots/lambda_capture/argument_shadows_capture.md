# META
~~~ini
description="A lambda argument shadows a variable from an outer scope. The lambda should use the argument, not the captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int OpenRound OpBar LowerIdent OpBar LowerIdent CloseRound OpenRound Int CloseRound LineComment CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (apply_anon
      (apply_anon
        (num_literal_i32 5)
        (lambda
          (body
            (lc "x")
          )
          (args
            (lc "x")
          )
        )
      )
      (num_literal_i32 10)
    )
  )
)
~~~
# FORMATTED
~~~roc
x = 5(|x| x)(10)# Should not capture outer `x` -- this should give a shadowing warning
~~~
# EXPECTED
DUPLICATE DEFINITION - argument_shadows_capture.md:3:7:3:8
UNUSED VARIABLE - argument_shadows_capture.md:2:5:2:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**argument_shadows_capture.md:2:10:3:5:**
```roc
    x = 5
    (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
