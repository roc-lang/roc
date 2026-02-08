# META
~~~ini
description=String interpolation with integer var
type=expr
~~~
# SOURCE
~~~roc
"zero: ${U64.to_str(0)}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "zero: "))
	(e-apply
		(e-ident (raw "U64.to_str"))
		(e-int (raw "0")))
	(e-string-part (raw "")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "zero: "))
	(e-call
		(e-lookup-external
			(builtin))
		(e-num (value "0")))
	(e-literal (string "")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
