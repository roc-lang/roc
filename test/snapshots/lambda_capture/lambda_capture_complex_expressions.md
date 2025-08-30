# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar KwIf LowerIdent OpGreaterThan Int OpenRound LowerIdent OpPlus LowerIdent CloseRound KwElse LowerIdent CloseRound OpenRound Int CloseRound OpenRound OpUnaryMinus Int CloseRound ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
inner
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:47 to 1:52

**Parse Error**
at 1:52 to 1:52

**Parse Error**
at 1:52 to 1:57

**Unsupported Node**
at 1:52 to 1:57

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
