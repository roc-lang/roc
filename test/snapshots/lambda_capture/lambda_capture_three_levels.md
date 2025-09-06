# META
~~~ini
description=Three-level nested lambda captures - innermost lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|outer| |middle| |inner| outer + middle + inner)(1)(2)(3)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound Int CloseRound OpenRound Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (apply_anon
      (lambda
        (body
          (lambda
            (body
              (lambda
                (body
                  (binop_plus
                    (binop_plus
                      (lc "outer")
                      (lc "middle")
                    )
                    (lc "inner")
                  )
                )
                (args
                  (lc "inner")
                )
              )
            )
            (args
              (lc "middle")
            )
          )
        )
        (args
          (lc "outer")
        )
      )
      (num_literal_i32 1)
    )
    (num_literal_i32 2)
  )
  (num_literal_i32 3)
)
~~~
# FORMATTED
~~~roc
(|outer| |middle| |inner| (outer + middle) + inner)(1)(2)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_three_levels.md:1:49:1:50:**
```roc
(|outer| |middle| |inner| outer + middle + inner)(1)(2)(3)
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
