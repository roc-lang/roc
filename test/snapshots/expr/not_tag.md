# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# TOKENS
~~~text
OpBang OpenRound UpperIdent OpenRound Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(unary_not <unary>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE MISMATCH - not_tag.md:1:1:1:8
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:7

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
