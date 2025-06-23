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
(file (1:1-17:49)
	(app (1:1-1:53)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:53)
			"pf"
			(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))
		(packages (1:13-1:53)
			(record_field (1:15-1:53)
				"pf"
				(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))))
	(statements
		(type_anno (4:1-5:15)
			"processComplex"
			(fn (4:18-4:72)
				(apply (4:18-4:61)
					(ty "Result")
					(apply (4:25-4:39)
						(ty "List")
						(apply (4:30-4:38)
							(ty "Maybe")
							(ty_var (4:36-4:37) "a")))
					(apply (4:41-4:60)
						(ty "Dict")
						(ty "Str")
						(apply (4:51-4:59)
							(ty "Error")
							(ty_var (4:57-4:58) "b"))))
				(apply (4:65-4:72)
					(ty "List")
					(ty_var (4:70-4:71) "a"))))
		(decl (5:1-6:9)
			(ident (5:1-5:15) "processComplex")
			(lambda (5:18-6:9)
				(args (ident (5:19-5:25) "result"))
				(ident (6:5-6:9) "" "when")))
		(ident (6:10-6:16) "" "result")
		(ident (6:17-6:19) "" "is")
		(malformed_stmt (7:9-7:27) "expected_colon_after_type_annotation")
		(list (7:26-7:28))
		(malformed_stmt (8:9-8:20) "expected_colon_after_type_annotation")
		(list (8:19-8:21))
		(type_anno (11:1-12:11)
			"deepNested"
			(fn (11:14-11:55)
				(apply (11:14-11:50)
					(ty "Maybe")
					(apply (11:20-11:49)
						(ty "Result")
						(apply (11:27-11:45)
							(ty "List")
							(apply (11:32-11:44)
								(ty "Dict")
								(ty "Str")
								(ty_var (11:42-11:43) "a")))
						(ty_var (11:47-11:48) "b")))
				(ty_var (11:54-11:55) "a")))
		(decl (12:1-12:25)
			(ident (12:1-12:11) "deepNested")
			(lambda (12:14-12:25)
				(args (underscore))
				(malformed_expr (12:18-12:25) "expr_unexpected_token")))
		(string (12:24-12:41) (string_part (12:25-12:40) "not implemented"))
		(type_decl (15:1-17:6)
			(header (15:1-15:18)
				"ComplexType"
				(args
					(ty_var (15:13-15:14) "a")
					(ty_var (15:16-15:17) "b")))
			(apply (15:21-15:64)
				(ty "Result")
				(apply (15:28-15:42)
					(ty "List")
					(apply (15:33-15:41)
						(ty "Maybe")
						(ty_var (15:39-15:40) "a")))
				(apply (15:44-15:63)
					(ty "Dict")
					(ty "Str")
					(apply (15:54-15:62)
						(ty "Error")
						(ty_var (15:60-15:61) "b")))))
		(decl (17:1-17:49)
			(ident (17:1-17:6) "main!")
			(lambda (17:9-17:49)
				(args (underscore))
				(apply (17:13-17:49)
					(ident (17:13-17:27) "" "processComplex")
					(apply (17:28-17:48)
						(tag (17:28-17:30) "Ok")
						(list (17:31-17:47)
							(apply (17:32-17:40)
								(tag (17:32-17:36) "Some")
								(int (17:37-17:39) "42"))
							(tag (17:42-17:46) "None"))))))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:15)
				(pid 98)
				(ident "processComplex")))
		(def_expr
			(e_lambda (5:18-6:9)
				(args
					(p_assign (5:19-5:25)
						(pid 99)
						(ident "result")))
				(e_runtime_error (6:5-6:9) "ident_not_in_scope")))
		(annotation (5:1-5:15)
			(signature 107)
			(declared_type
				(fn (4:18-4:72)
					(apply (4:18-4:61)
						"Result"
						(apply (4:25-4:39)
							"List"
							(apply (4:30-4:38)
								"Maybe"
								(ty_var (4:36-4:37) "a")))
						(apply (4:41-4:60)
							"Dict"
							(ty (4:46-4:49) "Str")
							(apply (4:51-4:59)
								"Error"
								(ty_var (4:57-4:58) "b"))))
					(apply (4:65-4:72)
						"List"
						(ty_var (4:70-4:71) "a"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (12:1-12:11)
				(pid 126)
				(ident "deepNested")))
		(def_expr
			(e_lambda (12:14-12:25)
				(args (p_underscore (12:15-12:16) (pid 127)))
				(e_runtime_error (12:18-12:25) "lambda_body_not_canonicalized")))
		(annotation (12:1-12:11)
			(signature 135)
			(declared_type
				(fn (11:14-11:55)
					(apply (11:14-11:50)
						"Maybe"
						(apply (11:20-11:49)
							"Result"
							(apply (11:27-11:45)
								"List"
								(apply (11:32-11:44)
									"Dict"
									(ty (11:37-11:40) "Str")
									(ty_var (11:42-11:43) "a")))
							(ty_var (11:47-11:48) "b")))
					(ty_var (11:54-11:55) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (17:1-17:6)
				(pid 139)
				(ident "main!")))
		(def_expr
			(e_lambda (17:9-17:49)
				(args (p_underscore (17:10-17:11) (pid 140)))
				(e_call (17:13-17:49)
					(e_lookup (17:13-17:27) (pid 98))
					(e_call (17:28-17:48)
						(e_tag (17:28-17:30)
							(ext_var 0)
							(name "Ok")
							(args "TODO"))
						(e_list (17:31-17:47)
							(elem_var 152)
							(elems
								(e_call (17:32-17:40)
									(e_tag (17:32-17:36)
										(ext_var 0)
										(name "Some")
										(args "TODO"))
									(e_int (17:37-17:39)
										(int_var 147)
										(precision_var 146)
										(literal "42")
										(value "TODO")
										(bound "u8")))
								(e_tag (17:42-17:46)
									(ext_var 0)
									(name "None")
									(args "TODO")))))))))
	(s_type_decl (15:1-17:6)
		(type_header (15:1-15:18)
			"ComplexType"
			(args
				(ty_var (15:13-15:14) "a")
				(ty_var (15:16-15:17) "b")))
		(apply (15:21-15:64)
			"Result"
			(apply (15:28-15:42)
				"List"
				(apply (15:33-15:41)
					"Maybe"
					(ty_var (15:39-15:40) "a")))
			(apply (15:44-15:63)
				"Dict"
				(ty (15:49-15:52) "Str")
				(apply (15:54-15:62)
					"Error"
					(ty_var (15:60-15:61) "b"))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processComplex" 109 (type "*"))
		(def "deepNested" 137 (type "*"))
		(def "main!" 157 (type "*")))
	(expressions
		(expr (5:18-6:9) 102 (type "*"))
		(expr (12:14-12:25) 130 (type "*"))
		(expr (17:9-17:49) 156 (type "*"))))
~~~