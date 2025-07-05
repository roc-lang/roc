# META
~~~ini
description=qualified_tag malformed
type=expr
~~~
# SOURCE
~~~roc
One.Two.Whee
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotUpperIdent(1:8-1:13),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.13 (raw "One.Two.Whee"))
~~~
# FORMATTED
~~~roc
Whee
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.13 (name "Whee"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "[Whee]*"))
~~~
