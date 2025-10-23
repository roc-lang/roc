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
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-record
		(field (field "foo")
			(e-string
				(e-string-part (raw "Hello")))))
	(e-ident (raw "foo")))
~~~
# FORMATTED
~~~roc
{ foo: "Hello" }.foo
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "foo")
	(receiver
		(e-record
			(fields
				(field (name "foo")
					(e-string
						(e-literal (string "Hello"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
