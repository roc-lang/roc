# META
~~~ini
description=Record with single field
type=expr
~~~
# SOURCE
~~~roc
{ name: "test" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "test")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "name")
			(e-string
				(e-literal (string "test"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ name: Str }"))
~~~
