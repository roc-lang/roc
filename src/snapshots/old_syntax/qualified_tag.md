# META
~~~ini
description=qualified_tag malformed
type=expr
~~~
# SOURCE
~~~roc
One.Two.Whee
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotUpperIdent(1:8-1:13),EndOfFile(1:13-1:13),
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
(e-tag @1.1-1.4 (ext-var 73) (name "One") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[One]*"))
~~~
