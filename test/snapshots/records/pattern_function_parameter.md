# META
~~~ini
description=Record pattern destructuring in function parameter
type=statement
~~~
# SOURCE
~~~roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
~~~
# TOKENS
~~~text
LowerIdent OpAssign OpBar OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly OpBar String ~~~
# PARSE
~~~clojure
(empty)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
