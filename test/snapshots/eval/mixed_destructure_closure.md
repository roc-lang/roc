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
        (lc "a")
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
            (lc "d")
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
(|{ a, x: (b, c), y: { d, e: e } }| (((a + b) + c) + d) + e)({ a: 1, x: (2, 3), y: { d: 4, e: 5 } })
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


**UNDEFINED VARIABLE**
Nothing is named **e** in this scope.
Is there an **import** or **exposing** missing up-top?

**mixed_destructure_closure.md:1:49:1:50:**
```roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
```
                                                ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 51
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
(var #14 -> #15)
(var #15 -> #16)
(var #16 -> #17)
(var #17 -> #18)
(var #18 -> #19)
(var #19 -> #20)
(var #20 -> #21)
(var #21 -> #22)
(var #22 -> #42)
(var #23 -> #44)
(var #24 _)
(var #25 Num *)
(var #26 _)
(var #27 _)
(var #28 Num *)
(var #29 Num *)
(var #30 -> #45)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 Num *)
(var #35 _)
(var #36 _)
(var #37 Num *)
(var #38 _)
(var #39 -> #47)
(var #40 _)
(var #41 -> #49)
(var #42 _)
(var #43 -> #49)
(var #44 -> #50)
(var #45 tuple)
(var #46 {})
(var #47 record)
(var #48 {})
(var #49 record)
(var #50 fn_pure)
~~~
# TYPES
~~~roc
d : _f
a : _f
b : _f
c : _f
~~~
