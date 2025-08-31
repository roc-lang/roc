# META
~~~ini
description=Record as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent Comma LowerIdent CloseCurly OpBar LowerIdent OpStar LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_star
        (lc "x")
        (lc "y")
      )
    )
    (args
      (record_literal
        (binop_colon
          (lc "x")
          (lc "x")
        )
        (binop_colon
          (lc "y")
          (lc "y")
        )
      )
    )
  )
  (record_literal
    (binop_colon
      (lc "x")
      (num_literal_i32 10)
    )
    (binop_colon
      (lc "y")
      (num_literal_i32 20)
    )
  )
)
~~~
# FORMATTED
~~~roc
(|{ x : x, y : y }| x * y)({ x : 10, y : 20 })
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_argument_closure.md:1:3:1:11:**
```roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
```
  ^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
