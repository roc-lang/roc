# META
~~~ini
description=Dollar-prefixed record field names are preserved
type=expr
~~~
# SOURCE
~~~roc
{ $field : "value" }
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
	(field (field "$field")
		(e-string
			(e-string-part (raw "value")))))
~~~
# FORMATTED
~~~roc
{ $field: "value" }
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$field")
			(e-string
				(e-literal (string "value"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $field: Str }"))
~~~
