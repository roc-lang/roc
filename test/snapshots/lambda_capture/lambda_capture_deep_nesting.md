# META
~~~ini
description=Deep nesting with multiple captures - five-level nested lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|a| |b| |c| |d| |e| a + b + c + d + e)(1)(2)(3)(4)(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound Int CloseRound OpenRound Int CloseRound OpenRound Int CloseRound OpenRound Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (apply_anon
      (apply_anon
        (apply_anon
          (lambda
            (body
              (lambda
                (body
                  (lambda
                    (body
                      (lambda
                        (body
                          (lambda
                            (body
                              (binop_plus
                                (binop_plus
                                  (binop_plus
                                    (binop_plus
                                      (lc "a")
                                      (lc "b")
                                    )
                                    (lc "c")
                                  )
                                  (lc "d")
                                )
                                (lc "e")
                              )
                            )
                            (args
                              (lc "e")
                            )
                          )
                        )
                        (args
                          (lc "d")
                        )
                      )
                    )
                    (args
                      (lc "c")
                    )
                  )
                )
                (args
                  (lc "b")
                )
              )
            )
            (args
              (lc "a")
            )
          )
          (num_literal_i32 1)
        )
        (num_literal_i32 2)
      )
      (num_literal_i32 3)
    )
    (num_literal_i32 4)
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
(|a| |b| |c| |d| |e| (((a + b) + c) + d) + e)(1)(2)(3)(4)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_deep_nesting.md:1:39:1:40:**
```roc
(|a| |b| |c| |d| |e| a + b + c + d + e)(1)(2)(3)(4)(5)
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
