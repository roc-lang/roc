# META
~~~ini
description=Record containing a string field with field access
type=expr
~~~
# SOURCE
~~~roc
{foo: "Hello"}.foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:5),OpColon(1:5-1:6),StringStart(1:7-1:8),StringPart(1:8-1:13),StringEnd(1:13-1:14),CloseCurly(1:14-1:15),NoSpaceDotLowerIdent(1:15-1:19),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.19
	(e-record @1.1-1.15
		(field (field "foo")
			(e-string @1.7-1.14
				(e-string-part @1.8-1.13 (raw "Hello")))))
	(e-ident @1.15-1.19 (raw "foo")))
~~~
# FORMATTED
~~~roc
{ foo: "Hello" }.foo
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.19 (field "foo")
	(receiver
		(e-record @1.1-1.15
			(fields
				(field (name "foo")
					(e-string @1.7-1.14
						(e-literal @1.8-1.13 (string "Hello"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.19 (type "Str"))
~~~
