# META
~~~ini
description=Table literal with typed columns
type=expr
~~~
# SOURCE
~~~roc
table name : Str, age : U8 {
    "Bob", 12,
    "Alice", 17,
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpenCurly,
StringStart,StringPart,StringEnd,Comma,Int,Comma,
StringStart,StringPart,StringEnd,Comma,Int,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-table
	(columns
		(table-column (name "name")
			(ty (name "Str")))
		(table-column (name "age")
			(ty (name "U8"))))
	(rows
		(table-row
			(e-string
				(e-string-part (raw "Bob")))
			(e-int (raw "12")))
		(table-row
			(e-string
				(e-string-part (raw "Alice")))
			(e-int (raw "17")))))
~~~
# FORMATTED
~~~roc
table name : Str, age : U8 {
	"Bob", 12,
	"Alice", 17,
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "#1"))
		(e-list
			(elems
				(e-record
					(fields
						(field (name "name")
							(e-string
								(e-literal (string "Bob"))))
						(field (name "age")
							(e-num (value "12")))))
				(e-record
					(fields
						(field (name "name")
							(e-string
								(e-literal (string "Alice"))))
						(field (name "age")
							(e-num (value "17"))))))))
	(e-lookup-local
		(p-assign (ident "#1"))))
~~~
# TYPES
~~~clojure
(expr (type "List({ age: U8, name: Str })"))
~~~
