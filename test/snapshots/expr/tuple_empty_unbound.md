# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
# TOKENS
~~~text
OpenRound CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - tuple_empty_unbound.md:1:1:1:3
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:2

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
