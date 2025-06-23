# META
~~~ini
description=Record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30 }
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:21),OpColon(1:21-1:22),Int(1:23-1:25),CloseCurly(1:26-1:27),EndOfFile(1:27-1:27),
~~~
# PARSE
~~~clojure
(record (1:1-1:27)
	(field
		"name"
		(string (1:9-1:16) (string_part (1:10-1:15) "Alice")))
	(field "age" (int (1:23-1:25) "30")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:1) "not_implemented")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~