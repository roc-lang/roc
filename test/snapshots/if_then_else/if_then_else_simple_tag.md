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
  (condition     (binop_dot
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
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
~~~
