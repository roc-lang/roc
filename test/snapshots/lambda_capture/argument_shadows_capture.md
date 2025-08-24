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
OpenCurly LowerIdent OpAssign Int OpenRound OpBar LowerIdent OpBar LowerIdent CloseRound OpenRound Int CloseRound CloseCurly ~~~
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
NO CHANGE
~~~
# EXPECTED
DUPLICATE DEFINITION - argument_shadows_capture.md:3:7:3:8
UNUSED VARIABLE - argument_shadows_capture.md:2:5:2:6
# PROBLEMS
**Unsupported Node**
at 3:6 to 3:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
x : _a
~~~
