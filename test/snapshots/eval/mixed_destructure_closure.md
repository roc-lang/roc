# META
~~~ini
description=Mixed destructue patterns
type=expr
~~~
# SOURCE
~~~roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent Comma LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseCurly OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon OpenRound Int Comma Int CloseRound Comma LowerIdent OpColon OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
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
      (record_literal
        (binop_colon
          (lc "a")
          (lc "a")
        )
        (binop_colon
          (lc "x")
          (tuple_literal
            (lc "b")
            (lc "c")
          )
        )
        (binop_colon
          (lc "y")
          (record_literal
            (binop_colon
              (lc "d")
              (lc "d")
            )
            (binop_colon
              (lc "e")
              (lc "e")
            )
          )
        )
      )
    )
  )
  (record_literal
    (binop_colon
      (lc "a")
      (num_literal_i32 1)
    )
    (binop_colon
      (lc "x")
      (tuple_literal
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
    )
    (binop_colon
      (lc "y")
      (record_literal
        (binop_colon
          (lc "d")
          (num_literal_i32 4)
        )
        (binop_colon
          (lc "e")
          (num_literal_i32 5)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
(|{ a: a, x: (b, c), y: { d: d, e: e } }| (((a + b) + c) + d) + e)({ a: 1, x: (2, 3), y: { d: 4, e: 5 } })
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**mixed_destructure_closure.md:1:50:1:52:**
```roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
```
                                                 ^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 49
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 -> #18)
(var #18 -> #19)
(var #19 -> #20)
(var #20 -> #21)
(var #21 -> #22)
(var #22 -> #23)
(var #23 -> #24)
(var #24 -> #44)
(var #25 -> #46)
(var #26 _)
(var #27 Num *)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 Num *)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 Num *)
(var #37 _)
(var #38 _)
(var #39 Num *)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 -> #47)
(var #44 _)
(var #45 -> #47)
(var #46 -> #48)
(var #47 {})
(var #48 fn_pure)
~~~
# TYPES
~~~roc
d : _f
a : _f
e : _f
b : _f
c : _f
~~~
