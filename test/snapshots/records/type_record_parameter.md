# META
~~~ini
description=Function type annotation with record parameter
type=statement
~~~
# SOURCE
~~~roc
process_things : { name : Str, age : U32, thing: a }, (a -> Str) -> Str
~~~
# TOKENS
~~~text
LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon LowerIdent CloseCurly Comma OpenRound LowerIdent OpArrow UpperIdent CloseRound OpArrow UpperIdent ~~~
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
