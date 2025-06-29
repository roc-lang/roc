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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **|> Result** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_namespace.md:11:31:11:40:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                              ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `withDefault` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:48),
LowerIdent(4:1-4:8),OpColon(4:9-4:10),UpperIdent(4:11-4:15),NoSpaceOpenRound(4:15-4:16),LowerIdent(4:16-4:20),CloseRound(4:20-4:21),OpArrow(4:22-4:24),LowerIdent(4:25-4:29),Newline(1:1-1:1),
LowerIdent(5:1-5:8),OpAssign(5:9-5:10),OpBar(5:11-5:12),LowerIdent(5:12-5:16),OpBar(5:16-5:17),OpenCurly(5:18-5:19),Newline(1:1-1:1),
Newline(6:6-6:88),
LowerIdent(7:5-7:9),OpAssign(7:10-7:11),Int(7:12-7:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:6-9:84),
LowerIdent(10:5-10:11),OpColon(10:12-10:13),LowerIdent(10:14-10:18),Newline(1:1-1:1),
LowerIdent(11:5-11:11),OpAssign(11:12-11:13),UpperIdent(11:14-11:18),NoSpaceDotLowerIdent(11:18-11:24),NoSpaceOpenRound(11:24-11:25),LowerIdent(11:25-11:29),CloseRound(11:29-11:30),OpPizza(11:31-11:33),UpperIdent(11:34-11:40),NoSpaceDotLowerIdent(11:40-11:52),NoSpaceOpenRound(11:52-11:53),LowerIdent(11:53-11:57),CloseRound(11:57-11:58),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(13:5-13:11),Newline(1:1-1:1),
CloseCurly(14:1-14:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(16:1-16:6),OpAssign(16:7-16:8),OpBar(16:9-16:10),Underscore(16:10-16:11),OpBar(16:11-16:12),OpenCurly(16:13-16:14),CloseCurly(16:14-16:15),EndOfFile(16:15-16:15),
~~~
# PARSE
~~~clojure
(file @1.1-16.15
	(app @1.1-1.57
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.57 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.57 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-5.8 (name "process")
			(ty-fn @4.11-4.29
				(ty-apply @4.11-4.21
					(ty (name "List"))
					(ty-var @4.16-4.20 (raw "elem")))
				(ty-var @4.25-4.29 (raw "elem"))))
		(s-decl @5.1-14.2
			(p-ident @5.1-5.8 (raw "process"))
			(e-lambda @5.11-14.2
				(args
					(p-ident @5.12-5.16 (raw "list")))
				(e-block @5.18-14.2
					(statements
						(s-decl @7.5-7.14
							(p-ident @7.5-7.9 (raw "elem"))
							(e-int @7.12-7.14 (raw "42")))
						(s-type-anno @10.5-11.11 (name "result")
							(ty-var @10.14-10.18 (raw "elem")))
						(s-decl @11.5-11.30
							(p-ident @11.5-11.11 (raw "result"))
							(e-apply @11.14-11.30
								(e-ident @11.14-11.24 (qaul "List") (raw ".first"))
								(e-ident @11.25-11.29 (qaul "") (raw "list"))))
						(e-malformed @11.31-11.40 (reason "expr_unexpected_token"))
						(e-apply @11.34-11.58
							(e-ident @11.34-11.52 (qaul "Result") (raw ".withDefault"))
							(e-ident @11.53-11.57 (qaul "") (raw "elem")))
						(e-ident @13.5-13.11 (qaul "") (raw "result"))))))
		(s-decl @16.1-16.15
			(p-ident @16.1-16.6 (raw "main!"))
			(e-lambda @16.9-16.15
				(args
					(p-underscore))
				(e-record @16.13-16.15)))))
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
	(d-let (id 112)
		(p-assign @5.1-5.8 (ident "process") (id 79))
		(e-lambda @5.11-14.2 (id 104)
			(args
				(p-assign @5.12-5.16 (ident "list") (id 80)))
			(e-block @5.18-14.2
				(s-let @7.5-7.14
					(p-assign @7.5-7.9 (ident "elem") (id 81))
					(e-int @7.12-7.14 (value "42") (id 82)))
				(s-type-anno @10.5-11.11 (name "result")
					(ty-var @10.14-10.18 (name "elem")))
				(s-let @11.5-11.30
					(p-assign @11.5-11.11 (ident "result") (id 88))
					(e-call @11.14-11.30 (id 93)
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local @11.25-11.29
							(pattern (id 80)))))
				(s-expr @11.34-13.11
					(e-call @11.34-11.58
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local @11.53-11.57
							(pattern (id 81)))))
				(e-lookup-local @13.5-13.11
					(pattern (id 88)))))
		(annotation @5.1-5.8 (signature 110) (id 111)
			(declared-type
				(ty-fn @4.11-4.29 (effectful false)
					(ty-apply @4.11-4.21 (symbol "List")
						(ty-var @4.16-4.20 (name "elem")))
					(ty-var @4.25-4.29 (name "elem"))))))
	(d-let (id 118)
		(p-assign @16.1-16.6 (ident "main!") (id 113))
		(e-lambda @16.9-16.15 (id 117)
			(args
				(p-underscore @16.10-16.11 (id 114)))
			(e-empty_record @16.13-16.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "process") (def_var 112) (type "List -> elem"))
		(d_assign (name "main!") (def_var 118) (type "* ? {}")))
	(expressions
		(expr @5.11-14.2 (type "List -> elem"))
		(expr @16.9-16.15 (type "* ? {}"))))
~~~
