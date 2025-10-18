# META
~~~ini
description=Record with single field as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x }| x )({ x: -10 })
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,OpenCurly,LowerIdent,CloseCurly,OpBar,LowerIdent,CloseRound,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-record
					(field (name "x") (rest false))))
			(e-ident (raw "x"))))
	(e-record
		(field (field "x")
			(e-int (raw "-10")))))
~~~
# FORMATTED
~~~roc
(|{ x }| x)({ x: -10 })
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-record-destructure
				(destructs
					(record-destruct (label "x") (ident "x")
						(required
							(p-assign (ident "x")))))))
		(e-lookup-local
			(p-assign (ident "x"))))
	(e-record
		(fields
			(field (name "x")
				(e-num (value "-10"))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
