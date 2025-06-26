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
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:27),OpColon(2:27-2:28),StringStart(2:29-2:30),StringPart(2:30-2:40),StringEnd(2:40-2:41),Comma(2:41-2:42),Newline(1:1-1:1),
LowerIdent(3:5-3:13),OpColon(3:13-3:14),StringStart(3:15-3:16),StringPart(3:16-3:23),StringEnd(3:23-3:24),Comma(3:24-3:25),Newline(1:1-1:1),
LowerIdent(4:5-4:14),OpColon(4:14-4:15),StringStart(4:16-4:17),StringPart(4:17-4:22),StringEnd(4:22-4:23),Comma(4:23-4:24),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-record @1-1-5-2
	(field (field "field_with_underscores") (optional false)
		(e-string @2-29-2-41
			(e-string-part @2-30-2-40 (raw "underscore"))))
	(field (field "field123") (optional false)
		(e-string @3-15-3-24
			(e-string-part @3-16-3-23 (raw "numbers"))))
	(field (field "camelCase") (optional false)
		(e-string @4-16-4-23
			(e-string-part @4-17-4-22 (raw "camel")))))
~~~
# FORMATTED
~~~roc
{
	field_with_underscores: "underscore",
	field123: "numbers",
	camelCase: "camel",
}
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~