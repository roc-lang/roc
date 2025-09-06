# META
~~~ini
description="A pure lambda that takes an argument and does not capture from its environment."
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(10)
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
  (num_literal_i32 10)
)
~~~
# FORMATTED
~~~roc
(|x| x + 1)(10)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**no_capture.md:1:11:1:12:**
```roc
(|x| x + 1)(10)
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
