# META
~~~ini
description=Nominal-value destructuring patterns (Type.(pat)) in match branches and lambda args
type=snippet
~~~
# SOURCE
~~~roc
Distance := U64

unwrap : Distance -> U64
unwrap = |d| match d {
    Distance.(n) => n
}

from_arg : Distance -> U64
from_arg = |Distance.(n)| n
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Distance")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "unwrap")
			(ty-fn
				(ty (name "Distance"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "unwrap"))
			(e-lambda
				(args
					(p-ident (raw "d")))
				(e-match
					(e-ident (raw "d"))
					(branches
						(branch
							(p-tag (raw "Distance")
								(p-ident (raw "n")))
							(e-ident (raw "n")))))))
		(s-type-anno (name "from_arg")
			(ty-fn
				(ty (name "Distance"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "from_arg"))
			(e-lambda
				(args
					(p-tag (raw "Distance")
						(p-ident (raw "n"))))
				(e-ident (raw "n"))))))
~~~
# FORMATTED
~~~roc
Distance := U64

unwrap : Distance -> U64
unwrap = |d| match d {
	Distance.(n) => n
}

from_arg : Distance -> U64
from_arg = |Distance.(n)| n
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unwrap"))
		(e-lambda
			(args
				(p-assign (ident "d")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "d"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-assign (ident "n")))))
							(value
								(e-lookup-local
									(p-assign (ident "n")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Distance") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "from_arg"))
		(e-lambda
			(args
				(p-nominal
					(p-assign (ident "n"))))
			(e-lookup-local
				(p-assign (ident "n"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Distance") (local))
				(ty-lookup (name "U64") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Distance"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Distance -> U64"))
		(patt (type "Distance -> U64")))
	(type_decls
		(nominal (type "Distance")
			(ty-header (name "Distance"))))
	(expressions
		(expr (type "Distance -> U64"))
		(expr (type "Distance -> U64"))))
~~~
