# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variables nested in Result and List types
mapResult : Result(a, e), (a -> b) -> Result(b, e)
mapResult = |result, transform| {
    when result is
        Ok(value) -> Ok(transform(value))
        Err(error) -> Err(error)
}

# Type variables nested in Maybe and List
filterMaybes : List(Maybe(t)) -> List(t)
filterMaybes = |list| {
    List.keepOks(list)
}

main! = |_| {}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `is` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

**UNUSED VARIABLE**
Variable ``transform`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_transform` to suppress this warning.
The unused variable is declared here:
**type_var_nested.md:6:23:6:32:**
```roc
    when result is
```


**UNDEFINED VARIABLE**
Nothing is named `keepOks` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:49),
LowerIdent(4:1-4:10),OpColon(4:11-4:12),UpperIdent(4:13-4:19),NoSpaceOpenRound(4:19-4:20),LowerIdent(4:20-4:21),Comma(4:21-4:22),LowerIdent(4:23-4:24),CloseRound(4:24-4:25),Comma(4:25-4:26),OpenRound(4:27-4:28),LowerIdent(4:28-4:29),OpArrow(4:30-4:32),LowerIdent(4:33-4:34),CloseRound(4:34-4:35),OpArrow(4:36-4:38),UpperIdent(4:39-4:45),NoSpaceOpenRound(4:45-4:46),LowerIdent(4:46-4:47),Comma(4:47-4:48),LowerIdent(4:49-4:50),CloseRound(4:50-4:51),Newline(1:1-1:1),
LowerIdent(5:1-5:10),OpAssign(5:11-5:12),OpBar(5:13-5:14),LowerIdent(5:14-5:20),Comma(5:20-5:21),LowerIdent(5:22-5:31),OpBar(5:31-5:32),OpenCurly(5:33-5:34),Newline(1:1-1:1),
LowerIdent(6:5-6:9),LowerIdent(6:10-6:16),LowerIdent(6:17-6:19),Newline(1:1-1:1),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:17),CloseRound(7:17-7:18),OpArrow(7:19-7:21),UpperIdent(7:22-7:24),NoSpaceOpenRound(7:24-7:25),LowerIdent(7:25-7:34),NoSpaceOpenRound(7:34-7:35),LowerIdent(7:35-7:40),CloseRound(7:40-7:41),CloseRound(7:41-7:42),Newline(1:1-1:1),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),LowerIdent(8:13-8:18),CloseRound(8:18-8:19),OpArrow(8:20-8:22),UpperIdent(8:23-8:26),NoSpaceOpenRound(8:26-8:27),LowerIdent(8:27-8:32),CloseRound(8:32-8:33),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:42),
LowerIdent(12:1-12:13),OpColon(12:14-12:15),UpperIdent(12:16-12:20),NoSpaceOpenRound(12:20-12:21),UpperIdent(12:21-12:26),NoSpaceOpenRound(12:26-12:27),LowerIdent(12:27-12:28),CloseRound(12:28-12:29),CloseRound(12:29-12:30),OpArrow(12:31-12:33),UpperIdent(12:34-12:38),NoSpaceOpenRound(12:38-12:39),LowerIdent(12:39-12:40),CloseRound(12:40-12:41),Newline(1:1-1:1),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:21),OpBar(13:21-13:22),OpenCurly(13:23-13:24),Newline(1:1-1:1),
UpperIdent(14:5-14:9),NoSpaceDotLowerIdent(14:9-14:17),NoSpaceOpenRound(14:17-14:18),LowerIdent(14:18-14:22),CloseRound(14:22-14:23),Newline(1:1-1:1),
CloseCurly(15:1-15:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(17:1-17:6),OpAssign(17:7-17:8),OpBar(17:9-17:10),Underscore(17:10-17:11),OpBar(17:11-17:12),OpenCurly(17:13-17:14),CloseCurly(17:14-17:15),EndOfFile(17:15-17:15),
~~~
# PARSE
~~~clojure
(file (1:1-17:15)
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
		(type_anno (4:1-5:10)
			"mapResult"
			(fn (4:13-4:51)
				(apply (4:13-4:25)
					(ty "Result")
					(ty_var (4:20-4:21) "a")
					(ty_var (4:23-4:24) "e"))
				(fn (4:28-4:34)
					(ty_var (4:28-4:29) "a")
					(ty_var (4:33-4:34) "b"))
				(apply (4:39-4:51)
					(ty "Result")
					(ty_var (4:46-4:47) "b")
					(ty_var (4:49-4:50) "e"))))
		(decl (5:1-9:2)
			(ident (5:1-5:10) "mapResult")
			(lambda (5:13-9:2)
				(args
					(ident (5:14-5:20) "result")
					(ident (5:22-5:31) "transform"))
				(block (5:33-9:2)
					(statements
						(ident (6:5-6:9) "" "when")
						(ident (6:10-6:16) "" "result")
						(ident (6:17-6:19) "" "is")
						(local_dispatch (7:9-8:12)
							(apply (7:9-7:18)
								(tag (7:9-7:11) "Ok")
								(ident (7:12-7:17) "" "value"))
							(apply (7:19-7:42)
								(tag (7:22-7:24) "Ok")
								(apply (7:25-7:41)
									(ident (7:25-7:34) "" "transform")
									(ident (7:35-7:40) "" "value"))))
						(local_dispatch (8:9-9:2)
							(apply (8:9-8:19)
								(tag (8:9-8:12) "Err")
								(ident (8:13-8:18) "" "error"))
							(apply (8:20-8:33)
								(tag (8:23-8:26) "Err")
								(ident (8:27-8:32) "" "error")))))))
		(type_anno (12:1-13:13)
			"filterMaybes"
			(fn (12:16-12:41)
				(apply (12:16-12:30)
					(ty "List")
					(apply (12:21-12:29)
						(ty "Maybe")
						(ty_var (12:27-12:28) "t")))
				(apply (12:34-12:41)
					(ty "List")
					(ty_var (12:39-12:40) "t"))))
		(decl (13:1-15:2)
			(ident (13:1-13:13) "filterMaybes")
			(lambda (13:16-15:2)
				(args (ident (13:17-13:21) "list"))
				(block (13:23-15:2)
					(statements
						(apply (14:5-14:23)
							(ident (14:5-14:17) "List" ".keepOks")
							(ident (14:18-14:22) "" "list"))))))
		(decl (17:1-17:15)
			(ident (17:1-17:6) "main!")
			(lambda (17:9-17:15)
				(args (underscore))
				(record (17:13-17:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variables nested in Result and List types
mapResult : Result(a, e), (a -> b) -> Result(b, e)
mapResult = |result, transform| {
	when
	result
	is
	Ok(value)->Ok(transform(value))
	Err(error)->Err(error)
}

# Type variables nested in Maybe and List
filterMaybes : List(Maybe(t)) -> List(t)
filterMaybes = |list| {
	List.keepOks(list)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:10)
				(pid 89)
				(ident "mapResult")))
		(def_expr
			(e_lambda (5:13-9:2)
				(args
					(p_assign (5:14-5:20)
						(pid 90)
						(ident "result"))
					(p_assign (5:22-5:31)
						(pid 91)
						(ident "transform")))
				(e_block (5:33-9:2)
					(s_expr (6:5-6:16) (e_runtime_error (6:5-6:9) "ident_not_in_scope"))
					(s_expr (6:10-6:19)
						(e_lookup (6:10-6:16) (pid 90)))
					(s_expr (6:17-7:11) (e_runtime_error (6:17-6:19) "ident_not_in_scope"))
					(s_expr (7:9-8:12) (e_runtime_error (1:1-1:1) "not_implemented"))
					(e_runtime_error (1:1-1:1) "not_implemented"))))
		(annotation (5:1-5:10)
			(signature 116)
			(declared_type
				(fn (4:13-4:51)
					(apply (4:13-4:25)
						"Result"
						(ty_var (4:20-4:21) "a")
						(ty_var (4:23-4:24) "e"))
					(parens (4:27-4:35)
						(fn (4:28-4:34)
							(ty_var (4:28-4:29) "a")
							(ty_var (4:33-4:34) "b")
							"false"))
					(apply (4:39-4:51)
						"Result"
						(ty_var (4:46-4:47) "b")
						(ty_var (4:49-4:50) "e"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (13:1-13:13)
				(pid 127)
				(ident "filterMaybes")))
		(def_expr
			(e_lambda (13:16-15:2)
				(args
					(p_assign (13:17-13:21)
						(pid 128)
						(ident "list")))
				(e_block (13:23-15:2)
					(e_call (14:5-14:23)
						(e_runtime_error (14:5-14:17) "ident_not_in_scope")
						(e_lookup (14:18-14:22) (pid 128))))))
		(annotation (13:1-13:13)
			(signature 138)
			(declared_type
				(fn (12:16-12:41)
					(apply (12:16-12:30)
						"List"
						(apply (12:21-12:29)
							"Maybe"
							(ty_var (12:27-12:28) "t")))
					(apply (12:34-12:41)
						"List"
						(ty_var (12:39-12:40) "t"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (17:1-17:6)
				(pid 141)
				(ident "main!")))
		(def_expr
			(e_lambda (17:9-17:15)
				(args (p_underscore (17:10-17:11) (pid 142)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "mapResult" 118 (type "*"))
		(def "filterMaybes" 140 (type "*"))
		(def "main!" 146 (type "*")))
	(expressions
		(expr (5:13-9:2) 106 (type "*"))
		(expr (13:16-15:2) 134 (type "*"))
		(expr (17:9-17:15) 145 (type "*"))))
~~~