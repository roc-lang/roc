# META
~~~ini
description=Punned single-field record nested directly as a record field value
type=expr
~~~
# SOURCE
~~~roc
{ params: { status } }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "params")
		(e-record
			(field (field "status")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "params")
			(e-record
				(fields
					(field (name "status")
						(e-runtime-error (tag "ident_not_in_scope"))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ params: { status: Error } }"))
~~~
