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
Int(1:1-1:2),
OpBang(2:1-2:2),
OpAmpersand(3:1-3:2),LowerIdent(3:2-3:3),NoSpaceDotLowerIdent(3:3-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "4"))
~~~
# FORMATTED
~~~roc
4
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "4"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(_size)"))
~~~
