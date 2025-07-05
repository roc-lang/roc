# META
~~~ini
description=Record with single field
type=expr
~~~
# SOURCE
~~~roc
{ name: "test" }
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:14),StringEnd(1:14-1:15),CloseCurly(1:16-1:17),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.17
	(field (field "name") (optional false)
		(e-string @1.9-1.15
			(e-string-part @1.10-1.14 (raw "test")))))
~~~
# FORMATTED
~~~roc
{name: "test"}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.17
	(fields
		(field (name "name")
			(e-string @1.9-1.15
				(e-literal @1.10-1.14 (string "test"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "{ name: Str }"))
~~~
