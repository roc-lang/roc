# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# TOKENS
~~~text
KwIf UpperIdent Dot UpperIdent UpperIdent OpenRound Int CloseRound KwElse UpperIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(if_else
  (condition     (binop_pipe
      (uc "Bool")
      (uc "True")
    )
)
  (then     (apply_uc
      (uc "Ok")
      (num_literal_i32 0)
    )
)
  (else     (apply_uc
      (uc "Err")
      (num_literal_i32 1)
    )
))
~~~
# FORMATTED
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
