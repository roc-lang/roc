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
(block
  (binop_equals
    (lc "message")
    (str_literal_big "Hello! Here are some emojis: \xf0\x9f\x91\xbb \xf0\x9f\x8e\x89 \xf0\x9f\x9a\x80")
  )
  (binop_equals
    (lc "greet")
    (str_literal_big "Welcome! caf\xc3\xa9 r\xc3\xa9sum\xc3\xa9 na\xc3\xafve \xe4\xbd\xa0\xe5\xa5\xbd \xe3\x81\x93\xe3\x82\x93\xe3\x81\xab\xe3\x81\xa1\xe3\x81\xaf \xce\xb1 \xce\xb2 \xce\xb3 \xe2\x88\x91 \xe2\x88\xab \xe2\x88\x9e")
  )
)
~~~
# FORMATTED
~~~roc
module [message, greet]

message = "Hello! Here are some emojis: 👻 🎉 🚀"

# Test other Unicode characters
greet = "Welcome! café résumé naïve 你好 こんにちは α β γ ∑ ∫ ∞"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
