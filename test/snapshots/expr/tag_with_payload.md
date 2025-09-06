# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# TOKENS
~~~text
UpperIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_uc
  (uc "Some")
  (num_literal_i32 42)
)
~~~
# FORMATTED
~~~roc
Some(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tag_applied)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 -> #4)
(var #2 Num *)
(var #3 _)
(var #4 fn_pure)
~~~
# TYPES
~~~roc
~~~
