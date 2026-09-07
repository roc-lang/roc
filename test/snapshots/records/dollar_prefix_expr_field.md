# META
~~~ini
description=Dollar-prefixed expression record field names are preserved
type=expr
~~~
# SOURCE
~~~roc
{ $name: "Ada" }
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
	(field (field "$name")
		(e-string
			(e-string-part (raw "Ada")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$name")
			(e-string
				(e-literal (string "Ada"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $name: Str }"))
~~~
