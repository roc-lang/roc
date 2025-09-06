# META
~~~ini
description=Open record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, .. } => Str
~~~
# TOKENS
~~~text
LowerIdent OpBang OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma DoubleDot CloseCurly OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(empty)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - type_open_record.md:1:42:1:44
PARSE ERROR - type_open_record.md:1:37:1:40
PARSE ERROR - type_open_record.md:1:47:1:49
MALFORMED TYPE - type_open_record.md:1:47:1:49
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
