# META
~~~ini
description=qualified_field
type=expr
~~~
# SOURCE
~~~roc
One.Two.rec.abc.def.ghi
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotLowerIdent(1:8-1:12),NoSpaceDotLowerIdent(1:12-1:16),NoSpaceDotLowerIdent(1:16-1:20),NoSpaceDotLowerIdent(1:20-1:24),EndOfFile(1:24-1:24),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.4 (raw "One"))
~~~
# FORMATTED
~~~roc
One
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.4 (name "One") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "[One]a"))
~~~
