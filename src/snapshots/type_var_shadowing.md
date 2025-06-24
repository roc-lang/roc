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
(file @1-1-13-15
	(app @1-1-1-57
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-57 (name "pf")
			(e-string @1-28-1-55
				(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))
		(packages @1-13-1-57
			(record-field @1-15-1-57 (name "pf")
				(e-string @1-28-1-55
					(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4-1-5-6 (name "outer")
			(ty-fn @4-9-4-15
				(ty-var @4-9-4-10 (raw "a"))
				(ty-var @4-14-4-15 (raw "a"))))
		(s-decl @5-1-8-12
			(p-ident @5-1-5-6 (raw "outer"))
			(e-lambda @5-9-8-12
				(args
					(p-ident @5-10-5-11 (raw "x")))
				(e-malformed @8-5-8-12 (reason "expected_expr_close_curly_or_comma"))))
		(e-malformed @8-11-8-14 (reason "expr_unexpected_token"))
		(e-lambda @8-13-8-18
			(args
				(p-ident @8-14-8-15 (raw "y")))
			(e-ident @8-17-8-18 (qaul "") (raw "y")))
		(e-apply @10-5-10-13
			(e-ident @10-5-10-10 (qaul "") (raw "inner"))
			(e-ident @10-11-10-12 (qaul "") (raw "x")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-decl @13-1-13-15
			(p-ident @13-1-13-6 (raw "main!"))
			(e-lambda @13-9-13-15
				(args
					(p-underscore))
				(e-record @13-13-13-15)))))
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
(can-ir
	(d-let (id 89)
		(p-assign @5-1-5-6 (ident "outer") (id 77))
		(e-lambda @5-9-8-12 (id 81)
			(args
				(p-assign @5-10-5-11 (ident "x") (id 78)))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @5-1-5-6 (signature 87) (id 88)
			(declared-type
				(ty-fn @4-9-4-15 (effectful false)
					(ty-var @4-9-4-10 (name "a"))
					(ty-var @4-14-4-15 (name "a"))))))
	(d-let (id 99)
		(p-assign @13-1-13-6 (ident "main!") (id 94))
		(e-lambda @13-9-13-15 (id 98)
			(args
				(p-underscore @13-10-13-11 (id 95)))
			(e-runtime-error (tag "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "outer") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @5-9-8-12 (type "*"))
		(expr @13-9-13-15 (type "*"))))
~~~