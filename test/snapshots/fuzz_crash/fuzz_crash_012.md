# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# TOKENS
~~~text
OpOr OpenRound OpBar OpenRound LowerIdent OpBar ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (lambda
      (body
        (malformed malformed:expr_unexpected_token)
      )
      (args
        (lc "l888888888")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
(|l888888888| )
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

**Parse Error**
at 1:16 to 1:16

**Parse Error**
at 1:17 to 1:17

**Parse Error**
at 1:3 to 1:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
