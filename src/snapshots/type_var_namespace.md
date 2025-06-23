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


**UNDECLARED TYPE VARIABLE**
The type variable ``elem`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**type_var_namespace.md:10:14:10:18:**
```roc
    result : elem
```


**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `withDefault` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

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
(file (1:1-16:15)
	(app (1:1-1:57)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:57)
			"pf"
			(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))
		(packages (1:13-1:57)
			(record_field (1:15-1:57)
				"pf"
				(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))))
	(statements
		(type_anno (4:1-5:8)
			"process"
			(fn (4:11-4:29)
				(apply (4:11-4:21)
					(ty "List")
					(ty_var (4:16-4:20) "elem"))
				(ty_var (4:25-4:29) "elem")))
		(decl (5:1-14:2)
			(ident (5:1-5:8) "process")
			(lambda (5:11-14:2)
				(args (ident (5:12-5:16) "list"))
				(block (5:18-14:2)
					(statements
						(decl (7:5-7:14)
							(ident (7:5-7:9) "elem")
							(int (7:12-7:14) "42"))
						(type_anno (10:5-11:11)
							"result"
							(ty_var (10:14-10:18) "elem"))
						(decl (11:5-11:30)
							(ident (11:5-11:11) "result")
							(apply (11:14-11:30)
								(ident (11:14-11:24) "List" ".first")
								(ident (11:25-11:29) "" "list")))
						(malformed_expr (11:31-11:40) "expr_unexpected_token")
						(apply (11:34-11:58)
							(ident (11:34-11:52) "Result" ".withDefault")
							(ident (11:53-11:57) "" "elem"))
						(ident (13:5-13:11) "" "result")))))
		(decl (16:1-16:15)
			(ident (16:1-16:6) "main!")
			(lambda (16:9-16:15)
				(args (underscore))
				(record (16:13-16:15))))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:8)
				(pid 78)
				(ident "process")))
		(def_expr
			(e_lambda (5:11-14:2)
				(args
					(p_assign (5:12-5:16)
						(pid 79)
						(ident "list")))
				(e_block (5:18-14:2)
					(s_let (7:5-7:14)
						(p_assign (7:5-7:9)
							(pid 80)
							(ident "elem"))
						(e_int (7:12-7:14)
							(int_var 82)
							(precision_var 81)
							(literal "42")
							(value "TODO")
							(bound "u8")))
					(s_let (11:5-11:30)
						(p_assign (11:5-11:11)
							(pid 89)
							(ident "result"))
						(e_call (11:14-11:30)
							(e_runtime_error (11:14-11:24) "ident_not_in_scope")
							(e_lookup (11:25-11:29) (pid 79))))
					(s_expr (11:34-13:11)
						(e_call (11:34-11:58)
							(e_runtime_error (11:34-11:52) "ident_not_in_scope")
							(e_lookup (11:53-11:57) (pid 80))))
					(e_lookup (13:5-13:11) (pid 89)))))
		(annotation (5:1-5:8)
			(signature 107)
			(declared_type
				(fn (4:11-4:29)
					(apply (4:11-4:21)
						"List"
						(ty_var (4:16-4:20) "elem"))
					(ty_var (4:25-4:29) "elem")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (16:1-16:6)
				(pid 110)
				(ident "main!")))
		(def_expr
			(e_lambda (16:9-16:15)
				(args (p_underscore (16:10-16:11) (pid 111)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "process" 109 (type "*"))
		(def "main!" 115 (type "*")))
	(expressions
		(expr (5:11-14:2) 102 (type "*"))
		(expr (16:9-16:15) 114 (type "*"))))
~~~