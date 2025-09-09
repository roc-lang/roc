# META
~~~ini
description=Record in let binding statement
type=statement
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, email: "alice@example.com" }
~~~
# TOKENS
~~~text
LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly ~~~
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
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
