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
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "field_with_underscores")
		(e-string
			(e-string-part (raw "underscore"))))
	(field (field "field123")
		(e-string
			(e-string-part (raw "numbers"))))
	(field (field "camelCase")
		(e-string
			(e-string-part (raw "camel")))))
~~~
# FORMATTED
~~~roc
{
	field_with_underscores: "underscore",
	field123: "numbers",
	camelCase: "camel",
}
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "field_with_underscores")
			(e-string
				(e-literal (string "underscore"))))
		(field (name "field123")
			(e-string
				(e-literal (string "numbers"))))
		(field (name "camelCase")
			(e-string
				(e-literal (string "camel"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ camelCase: Str, field123: Str, field_with_underscores: Str }"))
~~~
