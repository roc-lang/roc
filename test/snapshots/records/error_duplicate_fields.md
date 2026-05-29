# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice"))))
	(field (field "age")
		(e-int (raw "30")))
	(field (field "name")
		(e-string
			(e-string-part (raw "Bob"))))
	(field (field "email")
		(e-string
			(e-string-part (raw "alice@example.com"))))
	(field (field "age")
		(e-int (raw "25"))))
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
				(e-literal (string "Alice"))))
		(field (name "age")
			(e-num (value "30")))
		(field (name "email")
			(e-string
				(e-literal (string "alice@example.com"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Dec, email: Str, name: Str }"))
~~~
