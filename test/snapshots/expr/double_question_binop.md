# META
~~~ini
description=Double question default value
type=expr
~~~
# SOURCE
~~~roc
get_name!({}) ?? "Bob"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpDoubleQuestion,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "??")
	(e-apply
		(e-ident (raw "get_name!"))
		(e-record))
	(e-string
		(e-string-part (raw "Bob"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-empty_record)))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-lookup-local
						(p-assign (ident "#ok")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-string
						(e-literal (string "Bob"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
