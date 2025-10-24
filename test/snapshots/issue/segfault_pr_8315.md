# META
~~~ini
description=Regression test for segfault caused by self-capturing closure (PR #8315)
type=snippet
~~~
# SOURCE
~~~roc
# Minimal reproduction of segfault bug
# A closure definition that captures itself causes infinite recursion
# during closure construction in the interpreter

selfCapturing : {} -> U64
selfCapturing = |{}| selfCapturing({})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "selfCapturing")
			(ty-fn
				(ty-record)
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "selfCapturing"))
			(e-lambda
				(args
					(p-record))
				(e-apply
					(e-ident (raw "selfCapturing"))
					(e-record))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "selfCapturing"))
		(e-closure
			(captures
				(capture (ident "selfCapturing")))
			(e-lambda
				(args
					(p-record-destructure
						(destructs)))
				(e-call
					(e-lookup-local
						(p-assign (ident "selfCapturing")))
					(e-empty_record))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{  } -> Num(Int(Unsigned64))")))
	(expressions
		(expr (type "{  } -> Num(Int(Unsigned64))"))))
~~~
