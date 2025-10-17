# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field: a, other: _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,NamedUnderscore,CloseCurly,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "getField")
			(ty-fn
				(ty-record
					(anno-record-field (name "field")
						(ty-var (raw "a")))
					(anno-record-field (name "other")
						(underscore-ty-var (raw "_b"))))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "getField"))
			(e-lambda
				(args
					(p-ident (raw "record")))
				(e-field-access
					(e-ident (raw "record"))
					(e-ident (raw "field")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field : a, other : _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getField"))
		(e-lambda
			(args
				(p-assign (ident "record")))
			(e-dot-access (field "field")
				(receiver
					(e-lookup-local
						(p-assign (ident "record"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-record
						(field (field "field")
							(ty-rigid-var (name "a")))
						(field (field "other")
							(ty-rigid-var (name "_b"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ field: a, other: _b } -> a"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "{ field: a, other: _b } -> a"))
		(expr (type "_arg -> {}"))))
~~~
