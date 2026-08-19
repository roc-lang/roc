# META
~~~ini
description=Empty typed table literal
type=expr
~~~
# SOURCE
~~~roc
table name : Str, age : U8 {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpenCurly,CloseCurly,
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
	(rows))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "#1"))
		(e-empty_list))
	(e-lookup-local
		(p-assign (ident "#1"))))
~~~
# TYPES
~~~clojure
(expr (type "List({ age: U8, name: Str })"))
~~~
