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
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 30
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #6)
(var #6 -> #7)
(var #7 -> #8)
(var #8 -> #9)
(var #9 -> #10)
(var #10 -> #11)
(var #11 _)
(var #12 -> #25)
(var #13 -> #28)
(var #14 Num *)
(var #15 Num *)
(var #16 Num *)
(var #17 -> #27)
(var #18 -> #29)
(var #19 Num *)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 fn_pure)
(var #26 fn_pure)
(var #27 tuple)
(var #28 <error>)
(var #29 fn_pure)
~~~
# TYPES
~~~roc
a : _d
b : _d
c : _d
x : _d
~~~
