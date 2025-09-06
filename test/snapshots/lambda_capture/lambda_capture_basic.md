# META
~~~ini
description=Basic lambda capture detection during canonicalization
type=expr
~~~
# SOURCE
~~~roc
(|x| |y| x + y)(1)(2)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseRound OpenRound Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (lc "x")
              (lc "y")
            )
          )
          (args
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
      )
    )
    (num_literal_i32 1)
  )
  (num_literal_i32 2)
)
~~~
# FORMATTED
~~~roc
(|x| |y| x + y)(1)(2)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_basic.md:1:15:1:16:**
```roc
(|x| |y| x + y)(1)(2)
```
              ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 18
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 -> #5)
(var #5 -> #11)
(var #6 -> #14)
(var #7 -> #15)
(var #8 Num *)
(var #9 -> #17)
(var #10 Num *)
(var #11 _)
(var #12 -> #8)
(var #13 -> #10)
(var #14 -> #9)
(var #15 -> #16)
(var #16 fn_pure)
(var #17 fn_pure)
~~~
# TYPES
~~~roc
x : _a
y : _a
~~~
