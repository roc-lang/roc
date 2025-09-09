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
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 45
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #7)
(var #7 -> #8)
(var #8 -> #9)
(var #9 -> #10)
(var #10 -> #11)
(var #11 -> #12)
(var #12 -> #13)
(var #13 -> #14)
(var #14 -> #29)
(var #15 -> #35)
(var #16 -> #36)
(var #17 -> #37)
(var #18 -> #38)
(var #19 -> #39)
(var #20 Num *)
(var #21 -> #41)
(var #22 Num *)
(var #23 -> #42)
(var #24 Num *)
(var #25 -> #43)
(var #26 Num *)
(var #27 -> #44)
(var #28 Num *)
(var #29 _)
(var #30 -> #20)
(var #31 -> #22)
(var #32 -> #24)
(var #33 -> #26)
(var #34 -> #28)
(var #35 -> #27)
(var #36 -> #25)
(var #37 -> #23)
(var #38 -> #21)
(var #39 -> #40)
(var #40 fn_pure)
(var #41 fn_pure)
(var #42 fn_pure)
(var #43 fn_pure)
(var #44 fn_pure)
~~~
# TYPES
~~~roc
a : _f
b : _f
e : _f
c : _f
d : _f
~~~
