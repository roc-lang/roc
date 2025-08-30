# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma OpColon Int Comma Comma LowerIdent OpColon Comma LowerIdent UpperIdent Dot LowerIdent Comma String OpColon LowerIdent Comma Int OpColon String Comma OpColon CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
30
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:18 to 1:20

**Parse Error**
at 1:1 to 1:20

**Parse Error**
at 1:20 to 1:22

**Unsupported Node**
at 1:20 to 1:22

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
