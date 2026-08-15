# META
~~~ini
description=Untyped table literal of people
type=expr
~~~
# SOURCE
~~~roc
table name, age, favorite_color {
    "Bob", 12, "blue",
    "Alice", 17, "green",
    "Eve", 13, "red",
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpenCurly,
StringStart,StringPart,StringEnd,Comma,Int,Comma,StringStart,StringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,Int,Comma,StringStart,StringPart,StringEnd,Comma,
StringStart,StringPart,StringEnd,Comma,Int,Comma,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-table
	(columns
		(table-column (name "name"))
		(table-column (name "age"))
		(table-column (name "favorite_color")))
	(rows
		(table-row
			(e-string
				(e-string-part (raw "Bob")))
			(e-int (raw "12"))
			(e-string
				(e-string-part (raw "blue"))))
		(table-row
			(e-string
				(e-string-part (raw "Alice")))
			(e-int (raw "17"))
			(e-string
				(e-string-part (raw "green"))))
		(table-row
			(e-string
				(e-string-part (raw "Eve")))
			(e-int (raw "13"))
			(e-string
				(e-string-part (raw "red"))))))
~~~
# FORMATTED
~~~roc
table name, age, favorite_color {
	"Bob", 12, "blue",
	"Alice", 17, "green",
	"Eve", 13, "red",
}
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Bob"))))
				(field (name "age")
					(e-num (value "12")))
				(field (name "favorite_color")
					(e-string
						(e-literal (string "blue"))))))
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Alice"))))
				(field (name "age")
					(e-num (value "17")))
				(field (name "favorite_color")
					(e-string
						(e-literal (string "green"))))))
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Eve"))))
				(field (name "age")
					(e-num (value "13")))
				(field (name "favorite_color")
					(e-string
						(e-literal (string "red"))))))))
~~~
# TYPES
~~~clojure
(expr (type "List({ age: Dec, favorite_color: Str, name: Str })"))
~~~
