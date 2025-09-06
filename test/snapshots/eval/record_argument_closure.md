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
        (lc "x")
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
(|{ x, y: y }| x * y)({ x: 10, y: 20 })
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**record_argument_closure.md:1:18:1:19:**
```roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
```
                 ^


**UNDEFINED VARIABLE**
Nothing is named **y** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_argument_closure.md:1:17:1:18:**
```roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
```
                ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 21
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #6)
(var #6 -> #7)
(var #7 -> #16)
(var #8 -> #18)
(var #9 _)
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 Num *)
(var #14 _)
(var #15 -> #19)
(var #16 _)
(var #17 -> #19)
(var #18 -> #20)
(var #19 {})
(var #20 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
