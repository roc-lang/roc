# META
~~~ini
description=Record construction using mixed shorthand and explicit record fields
type=expr
~~~
# SOURCE
~~~roc
{ name, age: 30, email, status: "active", balance }
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:12),OpColon(1:12-1:13),Int(1:14-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:23),Comma(1:23-1:24),LowerIdent(1:25-1:31),OpColon(1:31-1:32),StringStart(1:33-1:34),StringPart(1:34-1:40),StringEnd(1:40-1:41),Comma(1:41-1:42),LowerIdent(1:43-1:50),CloseCurly(1:51-1:52),EndOfFile(1:52-1:52),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-52
	(field (field "name") (optional false))
	(field (field "age") (optional false)
		(e-int @1-14-1-16 (raw "30")))
	(field (field "email") (optional false))
	(field (field "status") (optional false)
		(e-string @1-33-1-41
			(e-string-part @1-34-1-40 (raw "active"))))
	(field (field "balance") (optional false)))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~