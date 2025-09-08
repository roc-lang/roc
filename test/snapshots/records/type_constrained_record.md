# META
~~~ini
description=Constrained record type annotation
type=statement
~~~
# SOURCE
~~~roc
process_user! : { name : Str, age : U32, ..a } => Str
~~~
# TOKENS
~~~text
LowerIdent OpBang OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma DoubleDot LowerIdent CloseCurly OpThinArrow UpperIdent ~~~
# PARSE
~~~clojure
(empty)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - type_constrained_record.md:1:42:1:44
PARSE ERROR - type_constrained_record.md:1:37:1:40
MALFORMED TYPE - type_constrained_record.md:1:37:1:45
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
