# META
~~~ini
description=Lambda function called with negative argument
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(-5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound OpenRound OpUnaryMinus Int CloseRound ~~~
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
  (unary_neg <unary_op>)
)
~~~
# FORMATTED
~~~roc
(|x| x + 1)(-5)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_with_negative_argument.md:1:11:1:12:**
```roc
(|x| x + 1)(-5)
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
