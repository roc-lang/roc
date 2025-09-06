# META
~~~ini
description=Double negative unary minus operation
type=expr
~~~
# SOURCE
~~~roc
(|x| -(-x))(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpUnaryMinus OpenRound OpUnaryMinus LowerIdent CloseRound CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (unary_neg <unary_op>)
    )
    (args
      (lc "x")
    )
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
(|x| --x)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**unary_minus_double_negative.md:1:7:1:12:**
```roc
(|x| -(-x))(5)
```
      ^^^^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 -> #7)
(var #3 -> #2)
(var #4 -> #2)
(var #5 -> #9)
(var #6 Num *)
(var #7 _)
(var #8 -> #6)
(var #9 -> #10)
(var #10 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
