# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'item' introduced in annotation
process : List(item), item -> item
process = |list, fallback| {
    # value identifier named 'item' is allowed - different namespace from type variable
    item = 42

    # type variable 'item' still refers to the function annotation's type parameter
    result : item
    result = List.first(list).ok_or(fallback)

    result
}

main! = |_| {}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_namespace.md:7:5:7:9
# PROBLEMS
**UNUSED VARIABLE**
Variable `item` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_item` to suppress this warning.
The unused variable is declared here:
**type_var_namespace.md:7:5:7:9:**
```roc
    item = 42
```
    ^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "process")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "item")))
				(ty-var (raw "item"))
				(ty-var (raw "item"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "list"))
					(p-ident (raw "fallback")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "item"))
							(e-int (raw "42")))
						(s-type-anno (name "result")
							(ty-var (raw "item")))
						(s-decl
							(p-ident (raw "result"))
							(e-field-access
								(e-apply
									(e-ident (raw "List.first"))
									(e-ident (raw "list")))
								(e-apply
									(e-ident (raw "ok_or"))
									(e-ident (raw "fallback")))))
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
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'item' introduced in annotation
process : List(item), item -> item
process = |list, fallback| {
	# value identifier named 'item' is allowed - different namespace from type variable
	item = 42

	# type variable 'item' still refers to the function annotation's type parameter
	result : item
	result = List.first(list).ok_or(fallback)

	result
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "list"))
				(p-assign (ident "fallback")))
			(e-block
				(s-let
					(p-assign (ident "item"))
					(e-num (value "42")))
				(s-let
					(p-assign (ident "result"))
					(e-dot-access (field "ok_or")
						(receiver
							(e-call
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "list")))))
						(args
							(e-lookup-local
								(p-assign (ident "fallback"))))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "item")))
				(ty-rigid-var-lookup (ty-rigid-var (name "item")))
				(ty-rigid-var-lookup (ty-rigid-var (name "item"))))))
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
		(patt (type "List(item), item -> item"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(item), item -> item"))
		(expr (type "_arg -> {}"))))
~~~
