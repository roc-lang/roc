# META
~~~ini
description=Record with special character fields (ok case)
type=expr
~~~
# SOURCE
~~~roc
{
    field_with_underscores: "underscore",
    field123: "numbers",
    camelCase: "camel",
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:25),OpColon(1:25-1:26),StringStart(1:27-1:28),StringPart(1:28-1:38),StringEnd(1:38-1:39),Comma(1:39-1:40),LowerIdent(1:41-1:49),OpColon(1:49-1:50),StringStart(1:51-1:52),StringPart(1:52-1:59),StringEnd(1:59-1:60),Comma(1:60-1:61),LowerIdent(1:62-1:71),OpColon(1:71-1:72),StringStart(1:73-1:74),StringPart(1:74-1:79),StringEnd(1:79-1:80),CloseCurly(1:81-1:82),EndOfFile(1:82-1:82),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-82
	(field (field "field_with_underscores") (optional false)
		(e-string @1-27-1-39
			(e-string-part @1-28-1-38 (raw "underscore"))))
	(field (field "field123") (optional false)
		(e-string @1-51-1-60
			(e-string-part @1-52-1-59 (raw "numbers"))))
	(field (field "camelCase") (optional false)
		(e-string @1-73-1-80
			(e-string-part @1-74-1-79 (raw "camel")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~
