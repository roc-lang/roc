# META
~~~ini
description=comment_after_annotation
type=expr
~~~
# SOURCE
~~~roc
F:e#


q
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:5-1:5),
Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "F"))
~~~
# FORMATTED
~~~roc
F
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "F"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[F]*"))
~~~
