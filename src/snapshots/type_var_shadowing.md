# META
~~~ini
description=Type variable shadowing produces warning but is allowed
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
    # Inner function shadows outer 'a' with its own 'a'
    inner : a -> a
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_var_shadowing.md:8:5:8:12:**
```roc
    inner = |y| y
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= |** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_shadowing.md:8:11:8:14:**
```roc
    inner = |y| y
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_shadowing.md:11:1:11:1:**
```roc
}
```


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_shadowing.md:5:10:5:11:**
```roc
outer = |x| {
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

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:40),
LowerIdent(4:1-4:6),OpColon(4:7-4:8),LowerIdent(4:9-4:10),OpArrow(4:11-4:13),LowerIdent(4:14-4:15),Newline(1:1-1:1),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),OpBar(5:9-5:10),LowerIdent(5:10-5:11),OpBar(5:11-5:12),OpenCurly(5:13-5:14),Newline(1:1-1:1),
Newline(6:6-6:56),
LowerIdent(7:5-7:10),OpColon(7:11-7:12),LowerIdent(7:13-7:14),OpArrow(7:15-7:17),LowerIdent(7:18-7:19),Newline(1:1-1:1),
LowerIdent(8:5-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),LowerIdent(8:14-8:15),OpBar(8:15-8:16),LowerIdent(8:17-8:18),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(10:5-10:10),NoSpaceOpenRound(10:10-10:11),LowerIdent(10:11-10:12),CloseRound(10:12-10:13),Newline(1:1-1:1),
CloseCurly(11:1-11:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(13:1-13:6),OpAssign(13:7-13:8),OpBar(13:9-13:10),Underscore(13:10-13:11),OpBar(13:11-13:12),OpenCurly(13:13-13:14),CloseCurly(13:14-13:15),EndOfFile(13:15-13:15),
~~~
# PARSE
~~~clojure
(file (1:1-13:15)
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
		(type_anno (4:1-5:6)
			"outer"
			(fn (4:9-4:15)
				(ty_var (4:9-4:10) "a")
				(ty_var (4:14-4:15) "a")))
		(decl (5:1-8:12)
			(ident (5:1-5:6) "outer")
			(lambda (5:9-8:12)
				(args (ident (5:10-5:11) "x"))
				(malformed_expr (8:5-8:12) "expected_expr_close_curly_or_comma")))
		(malformed_expr (8:11-8:14) "expr_unexpected_token")
		(lambda (8:13-8:18)
			(args (ident (8:14-8:15) "y"))
			(ident (8:17-8:18) "" "y"))
		(apply (10:5-10:13)
			(ident (10:5-10:10) "" "inner")
			(ident (10:11-10:12) "" "x"))
		(malformed_expr (1:1-1:1) "expr_unexpected_token")
		(decl (13:1-13:15)
			(ident (13:1-13:6) "main!")
			(lambda (13:9-13:15)
				(args (underscore))
				(record (13:13-13:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Outer function with type variable 'a'
outer : a -> a
outer = |x|
	|y| y

inner(x)


main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:6)
				(pid 77)
				(ident "outer")))
		(def_expr
			(e_lambda (5:9-8:12)
				(args
					(p_assign (5:10-5:11)
						(pid 78)
						(ident "x")))
				(e_runtime_error (8:5-8:12) "lambda_body_not_canonicalized")))
		(annotation (5:1-5:6)
			(signature 87)
			(declared_type
				(fn (4:9-4:15)
					(ty_var (4:9-4:10) "a")
					(ty_var (4:14-4:15) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (13:1-13:6)
				(pid 94)
				(ident "main!")))
		(def_expr
			(e_lambda (13:9-13:15)
				(args (p_underscore (13:10-13:11) (pid 95)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "outer" 89 (type "*"))
		(def "main!" 99 (type "*")))
	(expressions
		(expr (5:9-8:12) 81 (type "*"))
		(expr (13:9-13:15) 98 (type "*"))))
~~~