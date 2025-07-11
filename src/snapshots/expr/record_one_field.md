# META
~~~ini
description=Single field record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),CloseCurly(1:17-1:18),EndOfFile(1:18-1:18),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.18
	(field (field "name")
		(e-string @1.9-1.16
			(e-string-part @1.10-1.15 (raw "Alice")))))
~~~
# FORMATTED
~~~roc
{name: "Alice"}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.18
	(fields
		(field (name "name")
			(e-string @1.9-1.16
				(e-literal @1.10-1.15 (string "Alice"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.18 (type "{ name: Str }"))
~~~
