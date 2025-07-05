# META
~~~ini
description=underscore_name_type_annotation fail
type=expr
~~~
# SOURCE
~~~roc
A
:_h
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpColon(2:1-2:2),NamedUnderscore(2:2-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "A"))
~~~
# FORMATTED
~~~roc
A
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "A"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[A]*"))
~~~
