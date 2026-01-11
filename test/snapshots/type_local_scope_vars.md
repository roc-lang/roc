# META
~~~ini
description=Function-local type variables in separate scopes
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

test_scoped : a, b -> a
test_scoped = |a, b| {
		# Beacuse we introduce `a` at an outer rank and `f` references `a`,
		# `f` is _not_ generalized
    f : a -> a
    f = |z| z

		# Because this as no annotation, it is generalized
    g = |z| z

		# No err because we correctly provide `a` as the arg
		result : a
		result = f(a)

		# Err because we incorrectly provide `b` as the arg
		_result2 : b
		_result2 = f(b)

		# No err because `f` is generalized
		_result3 : a
		_result3 = g(a)

		# No err because `f` is generalized
		_result4 : b
		_result4 = g(b)

		result
}

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - type_local_scope_vars.md:19:16:19:17
# PROBLEMS
**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**type_local_scope_vars.md:19:16:19:17:**
```roc
		_result2 = f(b)
```
		             ^

This argument has the type:

    b

But `f` needs the first argument to be:

    a

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
NamedUnderscore,OpColon,LowerIdent,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
NamedUnderscore,OpColon,LowerIdent,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
NamedUnderscore,OpColon,LowerIdent,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
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
		(s-type-anno (name "test_scoped")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "test_scoped"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-block
					(statements
						(s-type-anno (name "f")
							(ty-fn
								(ty-var (raw "a"))
								(ty-var (raw "a"))))
						(s-decl
							(p-ident (raw "f"))
							(e-lambda
								(args
									(p-ident (raw "z")))
								(e-ident (raw "z"))))
						(s-decl
							(p-ident (raw "g"))
							(e-lambda
								(args
									(p-ident (raw "z")))
								(e-ident (raw "z"))))
						(s-type-anno (name "result")
							(ty-var (raw "a")))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "f"))
								(e-ident (raw "a"))))
						(s-type-anno (name "_result2")
							(ty-var (raw "b")))
						(s-decl
							(p-ident (raw "_result2"))
							(e-apply
								(e-ident (raw "f"))
								(e-ident (raw "b"))))
						(s-type-anno (name "_result3")
							(ty-var (raw "a")))
						(s-decl
							(p-ident (raw "_result3"))
							(e-apply
								(e-ident (raw "g"))
								(e-ident (raw "a"))))
						(s-type-anno (name "_result4")
							(ty-var (raw "b")))
						(s-decl
							(p-ident (raw "_result4"))
							(e-apply
								(e-ident (raw "g"))
								(e-ident (raw "b"))))
						(e-ident (raw "result"))))))
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

test_scoped : a, b -> a
test_scoped = |a, b| {
	# Beacuse we introduce `a` at an outer rank and `f` references `a`,
	# `f` is _not_ generalized
	f : a -> a
	f = |z| z

	# Because this as no annotation, it is generalized
	g = |z| z

	# No err because we correctly provide `a` as the arg
	result : a
	result = f(a)

	# Err because we incorrectly provide `b` as the arg
	_result2 : b
	_result2 = f(b)

	# No err because `f` is generalized
	_result3 : a
	_result3 = g(a)

	# No err because `f` is generalized
	_result4 : b
	_result4 = g(b)

	result
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test_scoped"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-block
				(s-let
					(p-assign (ident "f"))
					(e-lambda
						(args
							(p-assign (ident "z")))
						(e-lookup-local
							(p-assign (ident "z")))))
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
							(p-assign (ident "f")))
						(e-lookup-local
							(p-assign (ident "a")))))
				(s-let
					(p-assign (ident "_result2"))
					(e-call
						(e-lookup-local
							(p-assign (ident "f")))
						(e-lookup-local
							(p-assign (ident "b")))))
				(s-let
					(p-assign (ident "_result3"))
					(e-call
						(e-lookup-local
							(p-assign (ident "g")))
						(e-lookup-local
							(p-assign (ident "a")))))
				(s-let
					(p-assign (ident "_result4"))
					(e-call
						(e-lookup-local
							(p-assign (ident "g")))
						(e-lookup-local
							(p-assign (ident "b")))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
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
		(patt (type "a, b -> a"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a, b -> a"))
		(expr (type "_arg -> {}"))))
~~~
