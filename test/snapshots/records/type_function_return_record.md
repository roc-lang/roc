# META
~~~ini
description=Function type annotation returning record
type=statement
~~~
# SOURCE
~~~roc
create_user! : Str, U32 => { name : Str, age : U32, id : U64, active : Bool }
~~~
# TOKENS
~~~text
LowerIdent OpBang OpColon UpperIdent Comma UpperIdent OpFatArrow OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly ~~~
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
