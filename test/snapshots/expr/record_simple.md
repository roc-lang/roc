# META
~~~ini
description=Record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice"))))
	(field (field "age")
		(e-int (raw "30"))))
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
			(e-num (value "30")))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Num(_size), name: Error }"))
~~~
