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
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**argument_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : _a
~~~
