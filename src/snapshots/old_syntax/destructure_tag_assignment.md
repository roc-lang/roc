# META
~~~ini
description=destructure_tag_assignment
type=expr
~~~
# SOURCE
~~~roc
Email str = Email "blah@example.com"
str
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),LowerIdent(1:7-1:10),OpAssign(1:11-1:12),UpperIdent(1:13-1:18),StringStart(1:19-1:20),StringPart(1:20-1:36),StringEnd(1:36-1:37),Newline(1:1-1:1),
LowerIdent(2:1-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.6 (raw "Email"))
~~~
# FORMATTED
~~~roc
Email
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.6 (name "Email"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "[Email]*"))
~~~
