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
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
