# META
~~~ini
description=Using Str.inspect on an open union type variable should work
type=snippet
~~~
# SOURCE
~~~roc
main_for_host : Try({}, [Exit(I32), ..others]) -> Str
main_for_host = |result|
    match result {
        Ok({}) => "ok"
        Err(Exit(code)) => Str.inspect(code)
        Err(other) => Str.inspect(other)
    }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "main_for_host")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Exit"))
								(ty (name "I32"))))
						(ty-var (raw "others"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "main_for_host"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-record))
							(e-string
								(e-string-part (raw "ok"))))
						(branch
							(p-tag (raw "Err")
								(p-tag (raw "Exit")
									(p-ident (raw "code"))))
							(e-apply
								(e-ident (raw "Str.inspect"))
								(e-ident (raw "code"))))
						(branch
							(p-tag (raw "Err")
								(p-ident (raw "other")))
							(e-apply
								(e-ident (raw "Str.inspect"))
								(e-ident (raw "other"))))))))))
~~~
# FORMATTED
~~~roc
main_for_host : Try({}, [Exit(I32), ..]) -> Str
main_for_host = |result|
	match result {
		Ok({}) => "ok"
		Err(Exit(code)) => Str.inspect(code)
		Err(other) => Str.inspect(other)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main_for_host"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "ok")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-call
									(e-lookup-external
										(builtin))
									(e-lookup-local
										(p-assign (ident "code"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-call
									(e-lookup-external
										(builtin))
									(e-lookup-local
										(p-assign (ident "other"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Exit")
							(ty-lookup (name "I32") (builtin)))
						(ty-rigid-var (name "others"))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Try({  }, [Exit(I32), ..others]) -> Str")))
	(expressions
		(expr (type "Try({  }, [Exit(I32), ..others]) -> Str"))))
~~~
