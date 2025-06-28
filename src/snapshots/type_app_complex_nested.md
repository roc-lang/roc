# META
~~~ini
description=Complex nested type applications in function annotation - focused test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
processComplex = |result|
    when result is
        Ok(maybeList) -> []
        Err(_) -> []

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), b)) -> a
deepNested = |_| crash "not implemented"

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# PROBLEMS
**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**type_app_complex_nested.md:7:9:7:12:**
```roc
        Ok(maybeList) -> []
```


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_app_complex_nested.md:8:13:8:15:**
```roc
        Err(_) -> []
```


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**type_app_complex_nested.md:8:9:8:13:**
```roc
        Err(_) -> []
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_app_complex_nested.md:12:18:12:25:**
```roc
deepNested = |_| crash "not implemented"
```


**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``result`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result` to suppress this warning.
The unused variable is declared here:
**type_app_complex_nested.md:5:19:5:25:**
```roc
processComplex = |result|
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_app_complex_nested.md:5:1:5:15:**
```roc
processComplex = |result|
```

It is of type:
    _Result -> List_

But you are trying to use it as:
    _Result ? Error_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_app_complex_nested.md:12:1:12:11:**
```roc
deepNested = |_| crash "not implemented"
```

It is of type:
    _Maybe -> a_

But you are trying to use it as:
    _Result -> ListMaybe ? Error_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:63),
LowerIdent(4:1-4:15),OpColon(4:16-4:17),UpperIdent(4:18-4:24),NoSpaceOpenRound(4:24-4:25),UpperIdent(4:25-4:29),NoSpaceOpenRound(4:29-4:30),UpperIdent(4:30-4:35),NoSpaceOpenRound(4:35-4:36),LowerIdent(4:36-4:37),CloseRound(4:37-4:38),CloseRound(4:38-4:39),Comma(4:39-4:40),UpperIdent(4:41-4:45),NoSpaceOpenRound(4:45-4:46),UpperIdent(4:46-4:49),Comma(4:49-4:50),UpperIdent(4:51-4:56),NoSpaceOpenRound(4:56-4:57),LowerIdent(4:57-4:58),CloseRound(4:58-4:59),CloseRound(4:59-4:60),CloseRound(4:60-4:61),OpArrow(4:62-4:64),UpperIdent(4:65-4:69),NoSpaceOpenRound(4:69-4:70),LowerIdent(4:70-4:71),CloseRound(4:71-4:72),Newline(1:1-1:1),
LowerIdent(5:1-5:15),OpAssign(5:16-5:17),OpBar(5:18-5:19),LowerIdent(5:19-5:25),OpBar(5:25-5:26),Newline(1:1-1:1),
LowerIdent(6:5-6:9),LowerIdent(6:10-6:16),LowerIdent(6:17-6:19),Newline(1:1-1:1),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:21),CloseRound(7:21-7:22),OpArrow(7:23-7:25),OpenSquare(7:26-7:27),CloseSquare(7:27-7:28),Newline(1:1-1:1),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),Underscore(8:13-8:14),CloseRound(8:14-8:15),OpArrow(8:16-8:18),OpenSquare(8:19-8:20),CloseSquare(8:20-8:21),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(10:2-10:34),
LowerIdent(11:1-11:11),OpColon(11:12-11:13),UpperIdent(11:14-11:19),NoSpaceOpenRound(11:19-11:20),UpperIdent(11:20-11:26),NoSpaceOpenRound(11:26-11:27),UpperIdent(11:27-11:31),NoSpaceOpenRound(11:31-11:32),UpperIdent(11:32-11:36),NoSpaceOpenRound(11:36-11:37),UpperIdent(11:37-11:40),Comma(11:40-11:41),LowerIdent(11:42-11:43),CloseRound(11:43-11:44),CloseRound(11:44-11:45),Comma(11:45-11:46),LowerIdent(11:47-11:48),CloseRound(11:48-11:49),CloseRound(11:49-11:50),OpArrow(11:51-11:53),LowerIdent(11:54-11:55),Newline(1:1-1:1),
LowerIdent(12:1-12:11),OpAssign(12:12-12:13),OpBar(12:14-12:15),Underscore(12:15-12:16),OpBar(12:16-12:17),KwCrash(12:18-12:23),StringStart(12:24-12:25),StringPart(12:25-12:40),StringEnd(12:40-12:41),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(14:2-14:39),
UpperIdent(15:1-15:12),NoSpaceOpenRound(15:12-15:13),LowerIdent(15:13-15:14),Comma(15:14-15:15),LowerIdent(15:16-15:17),CloseRound(15:17-15:18),OpColon(15:19-15:20),UpperIdent(15:21-15:27),NoSpaceOpenRound(15:27-15:28),UpperIdent(15:28-15:32),NoSpaceOpenRound(15:32-15:33),UpperIdent(15:33-15:38),NoSpaceOpenRound(15:38-15:39),LowerIdent(15:39-15:40),CloseRound(15:40-15:41),CloseRound(15:41-15:42),Comma(15:42-15:43),UpperIdent(15:44-15:48),NoSpaceOpenRound(15:48-15:49),UpperIdent(15:49-15:52),Comma(15:52-15:53),UpperIdent(15:54-15:59),NoSpaceOpenRound(15:59-15:60),LowerIdent(15:60-15:61),CloseRound(15:61-15:62),CloseRound(15:62-15:63),CloseRound(15:63-15:64),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(17:1-17:6),OpAssign(17:7-17:8),OpBar(17:9-17:10),Underscore(17:10-17:11),OpBar(17:11-17:12),LowerIdent(17:13-17:27),NoSpaceOpenRound(17:27-17:28),UpperIdent(17:28-17:30),NoSpaceOpenRound(17:30-17:31),OpenSquare(17:31-17:32),UpperIdent(17:32-17:36),NoSpaceOpenRound(17:36-17:37),Int(17:37-17:39),CloseRound(17:39-17:40),Comma(17:40-17:41),UpperIdent(17:42-17:46),CloseSquare(17:46-17:47),CloseRound(17:47-17:48),CloseRound(17:48-17:49),EndOfFile(17:49-17:49),
~~~
# PARSE
~~~clojure
(file @1-1-17-49
	(app @1-1-1-53
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-53 (name "pf")
			(e-string @1-28-1-51
				(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))
		(packages @1-13-1-53
			(record-field @1-15-1-53 (name "pf")
				(e-string @1-28-1-51
					(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @4-1-5-15 (name "processComplex")
			(ty-fn @4-18-4-72
				(ty-apply @4-18-4-61
					(ty (name "Result"))
					(ty-apply @4-25-4-39
						(ty (name "List"))
						(ty-apply @4-30-4-38
							(ty (name "Maybe"))
							(ty-var @4-36-4-37 (raw "a"))))
					(ty-apply @4-41-4-60
						(ty (name "Dict"))
						(ty (name "Str"))
						(ty-apply @4-51-4-59
							(ty (name "Error"))
							(ty-var @4-57-4-58 (raw "b")))))
				(ty-apply @4-65-4-72
					(ty (name "List"))
					(ty-var @4-70-4-71 (raw "a")))))
		(s-decl @5-1-6-9
			(p-ident @5-1-5-15 (raw "processComplex"))
			(e-lambda @5-18-6-9
				(args
					(p-ident @5-19-5-25 (raw "result")))
				(e-ident @6-5-6-9 (qaul "") (raw "when"))))
		(e-ident @6-10-6-16 (qaul "") (raw "result"))
		(e-ident @6-17-6-19 (qaul "") (raw "is"))
		(s-malformed @7-9-7-27 (tag "expected_colon_after_type_annotation"))
		(e-list @7-26-7-28)
		(s-malformed @8-9-8-20 (tag "expected_colon_after_type_annotation"))
		(e-list @8-19-8-21)
		(s-type-anno @11-1-12-11 (name "deepNested")
			(ty-fn @11-14-11-55
				(ty-apply @11-14-11-50
					(ty (name "Maybe"))
					(ty-apply @11-20-11-49
						(ty (name "Result"))
						(ty-apply @11-27-11-45
							(ty (name "List"))
							(ty-apply @11-32-11-44
								(ty (name "Dict"))
								(ty (name "Str"))
								(ty-var @11-42-11-43 (raw "a"))))
						(ty-var @11-47-11-48 (raw "b"))))
				(ty-var @11-54-11-55 (raw "a"))))
		(s-decl @12-1-12-25
			(p-ident @12-1-12-11 (raw "deepNested"))
			(e-lambda @12-14-12-25
				(args
					(p-underscore))
				(e-malformed @12-18-12-25 (reason "expr_unexpected_token"))))
		(e-string @12-24-12-41
			(e-string-part @12-25-12-40 (raw "not implemented")))
		(s-type-decl @15-1-17-6
			(header @15-1-15-18 (name "ComplexType")
				(args
					(ty-var @15-13-15-14 (raw "a"))
					(ty-var @15-16-15-17 (raw "b"))))
			(ty-apply @15-21-15-64
				(ty (name "Result"))
				(ty-apply @15-28-15-42
					(ty (name "List"))
					(ty-apply @15-33-15-41
						(ty (name "Maybe"))
						(ty-var @15-39-15-40 (raw "a"))))
				(ty-apply @15-44-15-63
					(ty (name "Dict"))
					(ty (name "Str"))
					(ty-apply @15-54-15-62
						(ty (name "Error"))
						(ty-var @15-60-15-61 (raw "b"))))))
		(s-decl @17-1-17-49
			(p-ident @17-1-17-6 (raw "main!"))
			(e-lambda @17-9-17-49
				(args
					(p-underscore))
				(e-apply @17-13-17-49
					(e-ident @17-13-17-27 (qaul "") (raw "processComplex"))
					(e-apply @17-28-17-48
						(e-tag @17-28-17-30 (raw "Ok"))
						(e-list @17-31-17-47
							(e-apply @17-32-17-40
								(e-tag @17-32-17-36 (raw "Some"))
								(e-int @17-37-17-39 (raw "42")))
							(e-tag @17-42-17-46 (raw "None")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(b))) -> List(a)
processComplex = |result|
	whenresultis
[]
[]

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), b)) -> a
deepNested = |_| "not implemented"

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 111)
		(p-assign @5-1-5-15 (ident "processComplex") (id 98))
		(e-lambda @5-18-6-9 (id 103)
			(args
				(p-assign @5-19-5-25 (ident "result") (id 99)))
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation @5-1-5-15 (signature 109) (id 110)
			(declared-type
				(ty-fn @4-18-4-72 (effectful false)
					(ty-apply @4-18-4-61 (symbol "Result")
						(ty-apply @4-25-4-39 (symbol "List")
							(ty-apply @4-30-4-38 (symbol "Maybe")
								(ty-var @4-36-4-37 (name "a"))))
						(ty-apply @4-41-4-60 (symbol "Dict")
							(ty @4-46-4-49 (name "Str"))
							(ty-apply @4-51-4-59 (symbol "Error")
								(ty-var @4-57-4-58 (name "b")))))
					(ty-apply @4-65-4-72 (symbol "List")
						(ty-var @4-70-4-71 (name "a")))))))
	(d-let (id 141)
		(p-assign @12-1-12-11 (ident "deepNested") (id 128))
		(e-lambda @12-14-12-25 (id 133)
			(args
				(p-underscore @12-15-12-16 (id 129)))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @12-1-12-11 (signature 139) (id 140)
			(declared-type
				(ty-fn @11-14-11-55 (effectful false)
					(ty-apply @11-14-11-50 (symbol "Maybe")
						(ty-apply @11-20-11-49 (symbol "Result")
							(ty-apply @11-27-11-45 (symbol "List")
								(ty-apply @11-32-11-44 (symbol "Dict")
									(ty @11-37-11-40 (name "Str"))
									(ty-var @11-42-11-43 (name "a"))))
							(ty-var @11-47-11-48 (name "b"))))
					(ty-var @11-54-11-55 (name "a"))))))
	(d-let (id 162)
		(p-assign @17-1-17-6 (ident "main!") (id 143))
		(e-lambda @17-9-17-49 (id 161)
			(args
				(p-underscore @17-10-17-11 (id 144)))
			(e-call @17-13-17-49
				(e-lookup-local @17-13-17-27
					(pattern (id 98)))
				(e-call @17-28-17-48
					(e-tag @17-28-17-30 (ext-var 0) (name "Ok") (args "TODO"))
					(e-list @17-31-17-47 (elem-var 152)
						(elems
							(e-call @17-32-17-40
								(e-tag @17-32-17-36 (ext-var 0) (name "Some") (args "TODO"))
								(e-int @17-37-17-39 (value "42")))
							(e-tag @17-42-17-46 (ext-var 0) (name "None") (args "TODO"))))))))
	(s-type-decl @15-1-17-6 (id 83)
		(ty-header @15-1-15-18 (name "ComplexType")
			(ty-args
				(ty-var @15-13-15-14 (name "a"))
				(ty-var @15-16-15-17 (name "b"))))
		(ty-apply @15-21-15-64 (symbol "Result")
			(ty-apply @15-28-15-42 (symbol "List")
				(ty-apply @15-33-15-41 (symbol "Maybe")
					(ty-var @15-39-15-40 (name "a"))))
			(ty-apply @15-44-15-63 (symbol "Dict")
				(ty @15-49-15-52 (name "Str"))
				(ty-apply @15-54-15-62 (symbol "Error")
					(ty-var @15-60-15-61 (name "b")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "processComplex") (def_var 111) (type "Error"))
		(d_assign (name "deepNested") (def_var 141) (type "Error"))
		(d_assign (name "main!") (def_var 162) (type "* ? *")))
	(expressions
		(expr @5-18-6-9 (type "Error"))
		(expr @12-14-12-25 (type "Error"))
		(expr @17-9-17-49 (type "* ? *"))))
~~~
