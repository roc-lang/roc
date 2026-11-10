# META
~~~ini
description=Required and optional JSON fields share the same format-owned error
type=snippet
~~~
# SOURCE
~~~roc
required : Str -> Try({ name : Str }, [InvalidJson(Str)])
required = |input| Json.parse(input)

optional : Str -> Try({ name : Try(Str, [Missing]) }, [InvalidJson(Str)])
optional = |input| Json.parse(input)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "required")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty-record
						(anno-record-field (name "name")
							(ty (name "Str"))))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "InvalidJson"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "required"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Json.parse"))
					(e-ident (raw "input")))))
		(s-type-anno (name "optional")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty-record
						(anno-record-field (name "name")
							(ty-apply
								(ty (name "Try"))
								(ty (name "Str"))
								(ty-tag-union
									(tags
										(ty (name "Missing")))))))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "InvalidJson"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "optional"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Json.parse"))
					(e-ident (raw "input")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "required"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-call (constraint-fn-var 330)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-record
						(field (field "name")
							(ty-lookup (name "Str") (builtin))))
					(ty-tag-union
						(ty-tag-name (name "InvalidJson")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "optional"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-call (constraint-fn-var 551)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-record
						(field (field "name")
							(ty-apply (name "Try") (builtin)
								(ty-lookup (name "Str") (builtin))
								(ty-tag-union
									(ty-tag-name (name "Missing"))))))
					(ty-tag-union
						(ty-tag-name (name "InvalidJson")
							(ty-lookup (name "Str") (builtin)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try({ name: Str }, [InvalidJson(Str)])"))
		(patt (type "Str -> Try({ name: Try(Str, [Missing]) }, [InvalidJson(Str)])")))
	(expressions
		(expr (type "Str -> Try({ name: Str }, [InvalidJson(Str)])"))
		(expr (type "Str -> Try({ name: Try(Str, [Missing]) }, [InvalidJson(Str)])"))))
~~~
