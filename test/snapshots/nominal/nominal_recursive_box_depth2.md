# META
~~~ini
description=Recursive nominal with nested Box at depth 2+ (regression test for layout cache by nominal identity)
type=snippet
~~~
# SOURCE
~~~roc
RichDoc := [PlainText(Str), Wrapped(Box(RichDoc))]

depth0 : RichDoc
depth0 = RichDoc.PlainText("hello")

depth1 : RichDoc
depth1 = RichDoc.Wrapped(Box.box(RichDoc.PlainText("one")))

depth2 : RichDoc
depth2 = RichDoc.Wrapped(Box.box(RichDoc.Wrapped(Box.box(RichDoc.PlainText("two")))))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,CloseRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "RichDoc")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "PlainText"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "Wrapped"))
						(ty-apply
							(ty (name "Box"))
							(ty (name "RichDoc")))))))
		(s-type-anno (name "depth0")
			(ty (name "RichDoc")))
		(s-decl
			(p-ident (raw "depth0"))
			(e-apply
				(e-tag (raw "RichDoc.PlainText"))
				(e-string
					(e-string-part (raw "hello")))))
		(s-type-anno (name "depth1")
			(ty (name "RichDoc")))
		(s-decl
			(p-ident (raw "depth1"))
			(e-apply
				(e-tag (raw "RichDoc.Wrapped"))
				(e-apply
					(e-ident (raw "Box.box"))
					(e-apply
						(e-tag (raw "RichDoc.PlainText"))
						(e-string
							(e-string-part (raw "one")))))))
		(s-type-anno (name "depth2")
			(ty (name "RichDoc")))
		(s-decl
			(p-ident (raw "depth2"))
			(e-apply
				(e-tag (raw "RichDoc.Wrapped"))
				(e-apply
					(e-ident (raw "Box.box"))
					(e-apply
						(e-tag (raw "RichDoc.Wrapped"))
						(e-apply
							(e-ident (raw "Box.box"))
							(e-apply
								(e-tag (raw "RichDoc.PlainText"))
								(e-string
									(e-string-part (raw "two")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "depth0"))
		(e-nominal (nominal "RichDoc")
			(e-tag (name "PlainText")
				(args
					(e-string
						(e-literal (string "hello"))))))
		(annotation
			(ty-lookup (name "RichDoc") (local))))
	(d-let
		(p-assign (ident "depth1"))
		(e-nominal (nominal "RichDoc")
			(e-tag (name "Wrapped")
				(args
					(e-call
						(e-lookup-external
							(builtin))
						(e-nominal (nominal "RichDoc")
							(e-tag (name "PlainText")
								(args
									(e-string
										(e-literal (string "one"))))))))))
		(annotation
			(ty-lookup (name "RichDoc") (local))))
	(d-let
		(p-assign (ident "depth2"))
		(e-nominal (nominal "RichDoc")
			(e-tag (name "Wrapped")
				(args
					(e-call
						(e-lookup-external
							(builtin))
						(e-nominal (nominal "RichDoc")
							(e-tag (name "Wrapped")
								(args
									(e-call
										(e-lookup-external
											(builtin))
										(e-nominal (nominal "RichDoc")
											(e-tag (name "PlainText")
												(args
													(e-string
														(e-literal (string "two"))))))))))))))
		(annotation
			(ty-lookup (name "RichDoc") (local))))
	(s-nominal-decl
		(ty-header (name "RichDoc"))
		(ty-tag-union
			(ty-tag-name (name "PlainText")
				(ty-lookup (name "Str") (builtin)))
			(ty-tag-name (name "Wrapped")
				(ty-apply (name "Box") (builtin)
					(ty-lookup (name "RichDoc") (local)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "RichDoc"))
		(patt (type "RichDoc"))
		(patt (type "RichDoc")))
	(type_decls
		(nominal (type "RichDoc")
			(ty-header (name "RichDoc"))))
	(expressions
		(expr (type "RichDoc"))
		(expr (type "RichDoc"))
		(expr (type "RichDoc"))))
~~~
