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
LowerIdent OpBang OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma DoubleDot LowerIdent CloseCurly OpFatArrow UpperIdent ~~~
# PARSE
~~~clojure
(lc "process_user")
~~~
# FORMATTED
~~~roc
process_user
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lookup "process_user")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_b")
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
