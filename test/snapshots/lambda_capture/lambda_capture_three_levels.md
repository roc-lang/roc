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
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 27
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 -> #7)
(var #7 -> #8)
(var #8 -> #17)
(var #9 -> #21)
(var #10 -> #22)
(var #11 -> #23)
(var #12 Num *)
(var #13 -> #25)
(var #14 Num *)
(var #15 -> #26)
(var #16 Num *)
(var #17 _)
(var #18 -> #12)
(var #19 -> #14)
(var #20 -> #16)
(var #21 -> #15)
(var #22 -> #13)
(var #23 -> #24)
(var #24 fn_pure)
(var #25 fn_pure)
(var #26 fn_pure)
~~~
# TYPES
~~~roc
outer : _a
middle : _a
inner : _a
~~~
