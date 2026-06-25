# META
~~~ini
description=An open string literal unreachable from a generalized def's signature is defaulted to Str at the generalization boundary, silently (string literals default to Str without a warning)
type=snippet
~~~
# SOURCE
~~~roc
g = |x| "a".concat(x)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".concat")
					(receiver
						(e-string
							(e-string-part (raw "a"))))
					(args
						(e-ident (raw "x"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "concat") (constraint-fn-var 31)
				(receiver
					(e-string
						(e-literal (string "a"))))
				(args
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Str"))))
~~~
