# META
~~~ini
description=Multiple uses of same type variable in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

pair : a -> (a, a)
pair = |x| (x, x)

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
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
		(s-type-anno (name "pair")
			(ty-fn
				(ty-var (raw "a"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "pair"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-tuple
					(e-ident (raw "x"))
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
		(p-assign (ident "pair"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-tuple
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
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
		(patt (type "a -> (a, a)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> (a, a)"))
		(expr (type "_arg -> {}"))))
~~~
