# META
~~~ini
description=Multiple type variables in a single type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
    (first, second) = pair
    (second, first)
}

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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
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
					(p-ident (raw "pair")))
				(e-block
					(statements
						(s-decl
							(p-tuple
								(p-ident (raw "first"))
								(p-ident (raw "second")))
							(e-ident (raw "pair")))
						(e-tuple
							(e-ident (raw "second"))
							(e-ident (raw "first")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
	(first, second) = pair
	(second, first)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swap"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-block
				(s-let
					(p-tuple
						(patterns
							(p-assign (ident "first"))
							(p-assign (ident "second"))))
					(e-lookup-local
						(p-assign (ident "pair"))))
				(e-tuple
					(elems
						(e-lookup-local
							(p-assign (ident "second")))
						(e-lookup-local
							(p-assign (ident "first")))))))
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
