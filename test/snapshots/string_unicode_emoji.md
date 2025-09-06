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
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpAssign String BlankLine LineComment LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "message")

    (lc "greet")
))
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

# Test that Unicode emojis are properly handled in string literals
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
  (Stmt.assign
    (pattern (Patt.ident "message"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "greet"))
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 Str)
(var #5 _)
(var #6 -> #7)
(var #7 Str)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
message : Str
greet : Str
~~~
