# META
~~~ini
description=Two distinct type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
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
		(s-type-anno (name "swap")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swap"))
			(e-lambda
				(args
					(p-tuple
						(p-ident (raw "x"))
						(p-ident (raw "y"))))
				(e-tuple
					(e-ident (raw "y"))
					(e-ident (raw "x")))))
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
		(p-assign (ident "swap"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-assign (ident "x"))
						(p-assign (ident "y")))))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "y")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-tuple
						(ty-rigid-var (name "a"))
						(ty-rigid-var (name "b")))
					(ty-tuple
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
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
		(patt (type "(a, b) -> (b, a)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a, b) -> (b, a)"))
		(expr (type "_arg -> {}"))))
~~~
