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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
else 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:20 to 1:25

**Unsupported Node**
at 1:20 to 1:25

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
