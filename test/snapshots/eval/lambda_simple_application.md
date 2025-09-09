# META
~~~ini
description=Simple lambda application evaluation
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
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
(|x| x + 1)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_simple_application.md:1:11:1:12:**
```roc
(|x| x + 1)(5)
```
          ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 -> #3)
(var #3 -> #4)
(var #4 -> #7)
(var #5 -> #9)
(var #6 Num *)
(var #7 Num *)
(var #8 -> #6)
(var #9 -> #10)
(var #10 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
