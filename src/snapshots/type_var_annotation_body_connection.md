# META
~~~ini
description=Type variable connection between function annotation and body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| {
    thing : a  # refers to the type var introduced in function type annotation
    thing = x  # refers to the value from the function parameter
    thing
}

main! = |_| {}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_var_annotation_body_connection.md:6:5:6:12:**
```roc
    thing = x  # refers to the value from the function parameter
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_annotation_body_connection.md:6:11:6:14:**
```roc
    thing = x  # refers to the value from the function parameter
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_annotation_body_connection.md:8:1:8:1:**
```roc
}
```


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_var_annotation_body_connection.md:4:13:4:14:**
```roc
identity = |x| {
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
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),LowerIdent(3:12-3:13),OpArrow(3:14-3:16),LowerIdent(3:17-3:18),Newline(1:1-1:1),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:14),OpBar(4:14-4:15),OpenCurly(4:16-4:17),Newline(1:1-1:1),
LowerIdent(5:5-5:10),OpColon(5:11-5:12),LowerIdent(5:13-5:14),Newline(5:17-5:79),
LowerIdent(6:5-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:14),Newline(6:17-6:65),
LowerIdent(7:5-7:10),Newline(1:1-1:1),
CloseCurly(8:1-8:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),OpBar(10:9-10:10),Underscore(10:10-10:11),OpBar(10:11-10:12),OpenCurly(10:13-10:14),CloseCurly(10:14-10:15),EndOfFile(10:15-10:15),
~~~
# PARSE
~~~clojure
(file (1:1-10:15)
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
		(type_anno (3:1-4:9)
			"identity"
			(fn (3:12-3:18)
				(ty_var (3:12-3:13) "a")
				(ty_var (3:17-3:18) "a")))
		(decl (4:1-6:12)
			(ident (4:1-4:9) "identity")
			(lambda (4:12-6:12)
				(args (ident (4:13-4:14) "x"))
				(malformed_expr (6:5-6:12) "expected_expr_close_curly_or_comma")))
		(malformed_expr (6:11-6:14) "expr_unexpected_token")
		(ident (6:13-6:14) "" "x")
		(ident (7:5-7:10) "" "thing")
		(malformed_expr (1:1-1:1) "expr_unexpected_token")
		(decl (10:1-10:15)
			(ident (10:1-10:6) "main!")
			(lambda (10:9-10:15)
				(args (underscore))
				(record (10:13-10:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| # refers to the type var introduced in function type annotation
	x # refers to the value from the function parameter
thing


main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:9)
				(pid 77)
				(ident "identity")))
		(def_expr
			(e_lambda (4:12-6:12)
				(args
					(p_assign (4:13-4:14)
						(pid 78)
						(ident "x")))
				(e_runtime_error (6:5-6:12) "lambda_body_not_canonicalized")))
		(annotation (4:1-4:9)
			(signature 87)
			(declared_type
				(fn (3:12-3:18)
					(ty_var (3:12-3:13) "a")
					(ty_var (3:17-3:18) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (10:1-10:6)
				(pid 94)
				(ident "main!")))
		(def_expr
			(e_lambda (10:9-10:15)
				(args (p_underscore (10:10-10:11) (pid 95)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "identity" 89 (type "*"))
		(def "main!" 99 (type "*")))
	(expressions
		(expr (4:12-6:12) 81 (type "*"))
		(expr (10:9-10:15) 98 (type "*"))))
~~~