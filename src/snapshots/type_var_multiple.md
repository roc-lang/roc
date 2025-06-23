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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **= pair** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_multiple.md:6:21:6:27:**
```roc
    (first, second) = pair
```


**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `second` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `second` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:63),
LowerIdent(4:1-4:5),OpColon(4:6-4:7),OpenRound(4:8-4:9),LowerIdent(4:9-4:10),Comma(4:10-4:11),LowerIdent(4:12-4:13),CloseRound(4:13-4:14),OpArrow(4:15-4:17),OpenRound(4:18-4:19),LowerIdent(4:19-4:20),Comma(4:20-4:21),LowerIdent(4:22-4:23),CloseRound(4:23-4:24),Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpBar(5:8-5:9),LowerIdent(5:9-5:13),OpBar(5:13-5:14),OpenCurly(5:15-5:16),Newline(1:1-1:1),
OpenRound(6:5-6:6),LowerIdent(6:6-6:11),Comma(6:11-6:12),LowerIdent(6:13-6:19),CloseRound(6:19-6:20),OpAssign(6:21-6:22),LowerIdent(6:23-6:27),Newline(1:1-1:1),
OpenRound(7:5-7:6),LowerIdent(7:6-7:12),Comma(7:12-7:13),LowerIdent(7:14-7:19),CloseRound(7:19-7:20),Newline(1:1-1:1),
CloseCurly(8:1-8:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),OpBar(10:9-10:10),Underscore(10:10-10:11),OpBar(10:11-10:12),OpenCurly(10:13-10:14),CloseCurly(10:14-10:15),EndOfFile(10:15-10:15),
~~~
# PARSE
~~~clojure
(file (1:1-10:15)
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
		(type_anno (4:1-5:5)
			"swap"
			(fn (4:8-4:24)
				(tuple (4:8-4:14)
					(ty_var (4:9-4:10) "a")
					(ty_var (4:12-4:13) "b"))
				(tuple (4:18-4:24)
					(ty_var (4:19-4:20) "b")
					(ty_var (4:22-4:23) "a"))))
		(decl (5:1-8:2)
			(ident (5:1-5:5) "swap")
			(lambda (5:8-8:2)
				(args (ident (5:9-5:13) "pair"))
				(block (5:15-8:2)
					(statements
						(tuple (6:5-6:20)
							(ident (6:6-6:11) "" "first")
							(ident (6:13-6:19) "" "second"))
						(malformed_expr (6:21-6:27) "expr_unexpected_token")
						(ident (6:23-6:27) "" "pair")
						(tuple (7:5-7:20)
							(ident (7:6-7:12) "" "second")
							(ident (7:14-7:19) "" "first"))))))
		(decl (10:1-10:15)
			(ident (10:1-10:6) "main!")
			(lambda (10:9-10:15)
				(args (underscore))
				(record (10:13-10:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
	(first, second)
	
	pair
	(second, first)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:5)
				(pid 83)
				(ident "swap")))
		(def_expr
			(e_lambda (5:8-8:2)
				(args
					(p_assign (5:9-5:13)
						(pid 84)
						(ident "pair")))
				(e_block (5:15-8:2)
					(s_expr (6:5-6:22)
						(e_tuple (6:5-6:20)
							(tuple_var "#89")
							(elems
								(e_runtime_error (6:6-6:11) "ident_not_in_scope")
								(e_runtime_error (6:13-6:19) "ident_not_in_scope"))))
					(s_expr (6:23-7:6)
						(e_lookup (6:23-6:27) (pid 84)))
					(e_tuple (7:5-7:20)
						(tuple_var "#98")
						(elems
							(e_runtime_error (7:6-7:12) "ident_not_in_scope")
							(e_runtime_error (7:14-7:19) "ident_not_in_scope"))))))
		(annotation (5:1-5:5)
			(signature 105)
			(declared_type
				(fn (4:8-4:24)
					(tuple (4:8-4:14)
						(ty_var (4:9-4:10) "a")
						(ty_var (4:12-4:13) "b"))
					(tuple (4:18-4:24)
						(ty_var (4:19-4:20) "b")
						(ty_var (4:22-4:23) "a"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (10:1-10:6)
				(pid 108)
				(ident "main!")))
		(def_expr
			(e_lambda (10:9-10:15)
				(args (p_underscore (10:10-10:11) (pid 109)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "swap" 107 (type "*"))
		(def "main!" 113 (type "*")))
	(expressions
		(expr (5:8-8:2) 101 (type "*"))
		(expr (10:9-10:15) 112 (type "*"))))
~~~