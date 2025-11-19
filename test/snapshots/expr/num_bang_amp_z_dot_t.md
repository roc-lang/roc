# META
~~~ini
description=num_bang_amp_z_dot_t
type=expr
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,
OpBang,
OpAmpersand,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "4"))
~~~
# FORMATTED
~~~roc
4
~~~
# CANONICALIZE
~~~clojure
(e-num (value "4"))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
