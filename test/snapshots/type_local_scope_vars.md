# META
~~~ini
description=Function-local type variables in separate scopes
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

fail : a -> a
fail = |x| {
    g : b -> b
    g = |z| z

		result : c
		result = g(x)

		result
}

pass : a -> a
pass = |x| {
    inner : a -> a
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - type_local_scope_vars.md:9:12:9:16
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_local_scope_vars.md:9:12:9:16:**
```roc
		result = g(x)
```
		         ^^^^

It has the type:
    _a_

But the type annotation says it should have the type:
    _c_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "fail")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "fail"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "g")
							(ty-fn
								(ty-var (raw "b"))
								(ty-var (raw "b"))))
						(s-decl
							(p-ident (raw "g"))
							(e-lambda
								(args
									(p-ident (raw "z")))
								(e-ident (raw "z"))))
						(s-type-anno (name "result")
							(ty-var (raw "c")))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "g"))
								(e-ident (raw "x"))))
						(e-ident (raw "result"))))))
		(s-type-anno (name "pass")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "pass"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-var (raw "a"))
								(ty-var (raw "a"))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-ident (raw "y")))
								(e-ident (raw "y"))))
						(e-apply
							(e-ident (raw "inner"))
							(e-ident (raw "x")))))))
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

fail : a -> a
fail = |x| {
	g : b -> b
	g = |z| z

	result : c
	result = g(x)

	result
}

pass : a -> a
pass = |x| {
	inner : a -> a
	inner = |y| y

	inner(x)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "fail"))
		(e-closure
			(captures
				(capture (ident "g")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-block
					(s-let
						(p-assign (ident "g"))
						(e-lambda
							(args
								(p-assign (ident "z")))
							(e-lookup-local
								(p-assign (ident "z")))))
					(s-let
						(p-assign (ident "result"))
						(e-call
							(e-lookup-local
								(p-assign (ident "g")))
							(e-lookup-local
								(p-assign (ident "x")))))
					(e-lookup-local
						(p-assign (ident "result"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "pass"))
		(e-closure
			(captures
				(capture (ident "inner")))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-block
					(s-let
						(p-assign (ident "inner"))
						(e-lambda
							(args
								(p-assign (ident "y")))
							(e-lookup-local
								(p-assign (ident "y")))))
					(e-call
						(e-lookup-local
							(p-assign (ident "inner")))
						(e-lookup-local
							(p-assign (ident "x")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
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
		(patt (type "Error -> Error"))
		(patt (type "a -> a"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "a -> a"))
		(expr (type "_arg -> {}"))))
~~~
