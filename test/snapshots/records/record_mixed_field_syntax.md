# META
~~~ini
description=Record construction using mixed shorthand and explicit record fields
type=expr
~~~
# SOURCE
~~~roc
{ name, age: 30, email, status: "active", balance }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name"))
	(field (field "age")
		(e-int (raw "30")))
	(field (field "email"))
	(field (field "status")
		(e-string
			(e-string-part (raw "active"))))
	(field (field "balance")))
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
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "age")
			(e-num (value "30")))
		(field (name "email")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "status")
			(e-string
				(e-literal (string "active"))))
		(field (name "balance")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Dec, balance: Error, email: Error, name: Error, status: Str }"))
~~~
