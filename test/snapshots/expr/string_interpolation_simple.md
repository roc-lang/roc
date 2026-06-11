# META
~~~ini
description=Simple string interpolation
type=expr
~~~
# SOURCE
~~~roc
"Hello ${name}!"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "Hello "))
	(e-ident (raw "name"))
	(e-string-part (raw "!")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "#interp_0"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-dispatch-call (method "from_interpolation") (constraint-fn-var 146)
		(receiver
			(e-string
				(e-literal (string "Hello "))))
		(args
			(e-dispatch-call (method "prepended") (constraint-fn-var 104)
				(receiver
					(e-dispatch-call (method "iter") (constraint-fn-var 40)
						(receiver
							(e-empty_list))
						(args)))
				(args
					(e-tuple
						(elems
							(e-lookup-local
								(p-assign (ident "#interp_0")))
							(e-string
								(e-literal (string "!"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
