# META
~~~ini
description=Explicitly typed F32 and F64 fractional literals are valid patterns
type=snippet
~~~
# SOURCE
~~~roc
classify32 : F32 -> I64
classify32 = |n| match n {
	1.5.F32 => 1
	_ => 0
}

classify64 : F64 -> I64
classify64 = |n| match n {
	1.5.F64 => 1
	_ => 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,NoSpaceDotUpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,NoSpaceDotUpperIdent,OpFatArrow,Int,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "classify32")
			(ty-fn
				(ty (name "F32"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "classify32"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-ident (raw "n"))
					(branches
						(branch
							(p-typed-frac (raw "1.5") (type "F32"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))
		(s-type-anno (name "classify64")
			(ty-fn
				(ty (name "F64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "classify64"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-ident (raw "n"))
					(branches
						(branch
							(p-typed-frac (raw "1.5") (type "F64"))
							(e-int (raw "1")))
						(branch
							(p-underscore)
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "classify32"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "n"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-small-dec)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "F32") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "classify64"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "n"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-small-dec)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "F64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F32 -> I64"))
		(patt (type "F64 -> I64")))
	(expressions
		(expr (type "F32 -> I64"))
		(expr (type "F64 -> I64"))))
~~~
