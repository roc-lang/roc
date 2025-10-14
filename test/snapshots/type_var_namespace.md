# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'elem' introduced in annotation
process : List(elem) -> elem
process = |list| {
    # value identifier named 'elem' is allowed - different namespace from type variable
    elem = 42

    # type variable 'elem' still refers to the function annotation's type parameter
    result : elem
    result = List.first(list) |> Result.withDefault(elem)

    result
}

main! = |_| {}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_var_namespace.md:11:31:11:33
UNDEFINED VARIABLE - type_var_namespace.md:11:14:11:24
UNRECOGNIZED SYNTAX - type_var_namespace.md:11:31:11:33
DOES NOT EXIST - type_var_namespace.md:11:34:11:52
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **|>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_namespace.md:11:31:11:33:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                              ^^


**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**type_var_namespace.md:11:14:11:24:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
             ^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**type_var_namespace.md:11:31:11:33:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                              ^^

This might be a syntax error, an unsupported language feature, or a typo.

**DOES NOT EXIST**
`Result.withDefault` does not exist.

**type_var_namespace.md:11:34:11:52:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                                 ^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpPizza,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
					(ty-var (raw "elem")))
				(ty-var (raw "elem"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "elem"))
							(e-int (raw "42")))
						(s-type-anno (name "result")
							(ty-var (raw "elem")))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "List.first"))
								(e-ident (raw "list"))))
						(e-malformed (reason "expr_unexpected_token"))
						(e-apply
							(e-ident (raw "Result.withDefault"))
							(e-ident (raw "elem")))
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

# Type variable 'elem' introduced in annotation
process : List(elem) -> elem
process = |list| {
	# value identifier named 'elem' is allowed - different namespace from type variable
	elem = 42

	# type variable 'elem' still refers to the function annotation's type parameter
	result : elem
	result = List.first(list)
		Result.withDefault(elem)

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
				(p-assign (ident "list")))
			(e-block
				(s-let
					(p-assign (ident "elem"))
					(e-num (value "42")))
				(s-let
					(p-assign (ident "result"))
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local
							(p-assign (ident "list")))))
				(s-expr
					(e-runtime-error (tag "expr_not_canonicalized")))
				(s-expr
					(e-call
						(e-runtime-error (tag "qualified_ident_does_not_exist"))
						(e-lookup-local
							(p-assign (ident "elem")))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "elem")))
				(ty-rigid-var-lookup (ty-rigid-var (name "elem"))))))
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
		(patt (type "List(elem) -> elem"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(elem) -> elem"))
		(expr (type "_arg -> {}"))))
~~~
