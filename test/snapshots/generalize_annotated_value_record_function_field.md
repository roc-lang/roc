# META
~~~ini
description=A non-expansive value whose type embeds a function (lambda) generalizes and instantiates at two concrete record types - the RFC's lambda-set-in-scheme hazard does not arise since lambda sets are not in the type system (tier-2)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

rec : { f : a -> a, n : List(b) }
rec = { f: |x| x, n: [] }

r1 : { f : U64 -> U64, n : List(U64) }
r1 = rec

r2 : { f : Str -> Str, n : List(Str) }
r2 = rec

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,LowerIdent,
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
		(s-type-anno (name "rec")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty-var (raw "a"))
						(ty-var (raw "a"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "b"))))))
		(s-decl
			(p-ident (raw "rec"))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))
				(field (field "n")
					(e-list))))
		(s-type-anno (name "r1")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "U64"))
						(ty (name "U64"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty (name "U64"))))))
		(s-decl
			(p-ident (raw "r1"))
			(e-ident (raw "rec")))
		(s-type-anno (name "r2")
			(ty-record
				(anno-record-field (name "f")
					(ty-fn
						(ty (name "Str"))
						(ty (name "Str"))))
				(anno-record-field (name "n")
					(ty-apply
						(ty (name "List"))
						(ty (name "Str"))))))
		(s-decl
			(p-ident (raw "r2"))
			(e-ident (raw "rec")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "rec"))
		(e-record
			(fields
				(field (name "f")
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "x")))))
				(field (name "n")
					(e-empty_list))))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-rigid-var (name "a"))
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
				(field (field "n")
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "r1"))
		(e-lookup-local
			(p-assign (ident "rec")))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-lookup (name "U64") (builtin))
						(ty-lookup (name "U64") (builtin))))
				(field (field "n")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "U64") (builtin)))))))
	(d-let
		(p-assign (ident "r2"))
		(e-lookup-local
			(p-assign (ident "rec")))
		(annotation
			(ty-record
				(field (field "f")
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(field (field "n")
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin)))))))
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
		(patt (type "{ f: a -> a, n: List(b) }"))
		(patt (type "{ f: U64 -> U64, n: List(U64) }"))
		(patt (type "{ f: Str -> Str, n: List(Str) }"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "{ f: a -> a, n: List(b) }"))
		(expr (type "{ f: U64 -> U64, n: List(U64) }"))
		(expr (type "{ f: Str -> Str, n: List(Str) }"))
		(expr (type "_arg -> {}"))))
~~~
