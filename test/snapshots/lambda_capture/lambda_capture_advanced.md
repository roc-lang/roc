# META
~~~ini
description=More davanced lambda capture
type=expr
~~~
# SOURCE
~~~roc
(|a, b, c| |x| a + b + c + x)(10, 20, 5)(7)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound Int Comma Int Comma Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (binop_plus
                (binop_plus
                  (lc "a")
                  (lc "b")
                )
                (lc "c")
              )
              (lc "x")
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (lc "a")
        (lc "b")
        (lc "c")
      )
    )
    (tuple_literal
      (num_literal_i32 10)
      (num_literal_i32 20)
      (num_literal_i32 5)
    )
  )
  (num_literal_i32 7)
)
~~~
# FORMATTED
~~~roc
(|a, b, c| |x| ((a + b) + c) + x)((10, 20, 5))(7)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_advanced.md:1:29:1:30:**
```roc
(|a, b, c| |x| a + b + c + x)(10, 20, 5)(7)
```
                            ^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
