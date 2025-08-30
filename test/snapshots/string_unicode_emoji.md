# META
~~~ini
description=Ensure string literals handle Unicode emojis and characters properly.
type=file
~~~
# SOURCE
~~~roc
module [message, greet]

# Test that Unicode emojis are properly handled in string literals
message = "Hello! Here are some emojis: 👻 🎉 🚀"

# Test other Unicode characters
greet = "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign String LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "message")

    (lc "greet")
))
~~~
# FORMATTED
~~~roc
module [message, greet]

message = "Hello! Here are some emojis: 👻 🎉 🚀"
greet = "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "message")
    (Expr.str_literal_big)
  )
  (Expr.binop_equals
    (Expr.lookup "greet")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
message : Str
greet : Str
~~~
