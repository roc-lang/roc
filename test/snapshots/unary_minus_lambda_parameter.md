# META
~~~ini
description=Unary minus operation on lambda parameter
type=expr
~~~
# SOURCE
~~~roc
(|x| -x)(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpUnaryMinus LowerIdent CloseRound OpenRound Int CloseRound ~~~
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
(|x| -x)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**unary_minus_lambda_parameter.md:1:7:1:9:**
```roc
(|x| -x)(5)
```
      ^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 -> #6)
(var #3 -> #2)
(var #4 -> #8)
(var #5 Num *)
(var #6 _)
(var #7 -> #5)
(var #8 -> #9)
(var #9 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
