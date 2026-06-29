# META
~~~ini
description=String interpolation can contain a record literal function argument
type=expr
~~~
# SOURCE
~~~roc
"${Str.inspect({ two: 2 })}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw ""))
	(e-apply
		(e-ident (raw "Str.inspect"))
		(e-record
			(field (field "two")
				(e-int (raw "2")))))
	(e-string-part (raw "")))
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
		(e-call (constraint-fn-var 62)
			(e-lookup-external
				(builtin))
			(e-record
				(fields
					(field (name "two")
						(e-num (value "2")))))))
	(e-interpolation (constraint-fn-var 117)
		(first
			(e-literal (string "")))
		(parts
			(e-lookup-local
				(p-assign (ident "#interp_0")))
			(e-literal (string "")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
