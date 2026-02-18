# META
~~~ini
description=Field access on record payload in tag union wrapped by opaque type (issue #8689)
type=snippet
~~~
# SOURCE
~~~roc
Wrapper := [
    Simple(Str),
    WithRecord({ name : Str }),
]

getName : Wrapper -> Str
getName = |w| match w {
    Wrapper.Simple(s) => s
    Wrapper.WithRecord(r) => r.name
}

expect getName(Wrapper.Simple("foo")) == "foo"
expect getName(Wrapper.WithRecord({ name: "hello" })) == "hello"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,CloseRound,Comma,
CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,CloseRound,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Wrapper")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Simple"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "WithRecord"))
						(ty-record
							(anno-record-field (name "name")
								(ty (name "Str"))))))))
		(s-type-anno (name "getName")
			(ty-fn
				(ty (name "Wrapper"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getName"))
			(e-lambda
				(args
					(p-ident (raw "w")))
				(e-match
					(e-ident (raw "w"))
					(branches
						(branch
							(p-tag (raw ".Simple")
								(p-ident (raw "s")))
							(e-ident (raw "s")))
						(branch
							(p-tag (raw ".WithRecord")
								(p-ident (raw "r")))
							(e-field-access
								(e-ident (raw "r"))
								(e-ident (raw "name"))))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "getName"))
					(e-apply
						(e-tag (raw "Wrapper.Simple"))
						(e-string
							(e-string-part (raw "foo")))))
				(e-string
					(e-string-part (raw "foo")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "getName"))
					(e-apply
						(e-tag (raw "Wrapper.WithRecord"))
						(e-record
							(field (field "name")
								(e-string
									(e-string-part (raw "hello")))))))
				(e-string
					(e-string-part (raw "hello")))))))
~~~
# FORMATTED
~~~roc
Wrapper := [
	Simple(Str),
	WithRecord({ name : Str }),
]

getName : Wrapper -> Str
getName = |w| match w {
	Wrapper.Simple(s) => s
	Wrapper.WithRecord(r) => r.name
}

expect getName(Wrapper.Simple("foo")) == "foo"
expect getName(Wrapper.WithRecord({ name: "hello" })) == "hello"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getName"))
		(e-lambda
			(args
				(p-assign (ident "w")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "w"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "s")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-dot-access (field "name")
									(receiver
										(e-lookup-local
											(p-assign (ident "r")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrapper") (local))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Wrapper"))
		(ty-tag-union
			(ty-tag-name (name "Simple")
				(ty-lookup (name "Str") (builtin)))
			(ty-tag-name (name "WithRecord")
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (builtin)))))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "getName")))
				(e-nominal (nominal "Wrapper")
					(e-tag (name "Simple")
						(args
							(e-string
								(e-literal (string "foo")))))))
			(e-string
				(e-literal (string "foo")))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "getName")))
				(e-nominal (nominal "Wrapper")
					(e-tag (name "WithRecord")
						(args
							(e-record
								(fields
									(field (name "name")
										(e-string
											(e-literal (string "hello"))))))))))
			(e-string
				(e-literal (string "hello"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Wrapper -> Str")))
	(type_decls
		(nominal (type "Wrapper")
			(ty-header (name "Wrapper"))))
	(expressions
		(expr (type "Wrapper -> Str"))))
~~~
