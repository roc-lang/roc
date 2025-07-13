# META
~~~ini
description=Record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:21),OpColon(1:21-1:22),Int(1:23-1:25),CloseCurly(1:26-1:27),EndOfFile(1:27-1:27),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.27
	(field (field "name")
		(e-string @1.9-1.16
			(e-string-part @1.10-1.15 (raw "Alice"))))
	(field (field "age")
		(e-int @1.23-1.25 (raw "30"))))
~~~
# FORMATTED
~~~roc
{name: "Alice", age: 30}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.27
	(fields
		(field (name "name")
			(e-string @1.9-1.16
				(e-literal @1.10-1.15 (string "Alice"))))
		(field (name "age")
			(e-int @1.23-1.25 (value "30")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.27 (type "{ name: Str, age: Num(a) }"))
~~~
