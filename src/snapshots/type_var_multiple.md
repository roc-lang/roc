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
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_var_multiple.md:6:21:6:27
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **= pair** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_var_multiple.md:6:21:6:27:**
```roc
    (first, second) = pair
```
                    ^^^^^^


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
(file @1.1-10.15
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
		(s-type-anno @4.1-5.5 (name "swap")
			(ty-fn @4.8-4.24
				(ty-tuple @4.8-4.14
					(ty-var @4.9-4.10 (raw "a"))
					(ty-var @4.12-4.13 (raw "b")))
				(ty-tuple @4.18-4.24
					(ty-var @4.19-4.20 (raw "b"))
					(ty-var @4.22-4.23 (raw "a")))))
		(s-decl @5.1-8.2
			(p-ident @5.1-5.5 (raw "swap"))
			(e-lambda @5.8-8.2
				(args
					(p-ident @5.9-5.13 (raw "pair")))
				(e-block @5.15-8.2
					(statements
						(e-tuple @6.5-6.20
							(e-ident @6.6-6.11 (raw "first"))
							(e-ident @6.13-6.19 (raw "second")))
						(e-malformed @6.21-6.27 (reason "expr_unexpected_token"))
						(e-ident @6.23-6.27 (raw "pair"))
						(e-tuple @7.5-7.20
							(e-ident @7.6-7.12 (raw "second"))
							(e-ident @7.14-7.19 (raw "first")))))))
		(s-decl @10.1-10.15
			(p-ident @10.1-10.6 (raw "main!"))
			(e-lambda @10.9-10.15
				(args
					(p-underscore))
				(e-record @10.13-10.15)))))
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
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "swap"))
		(e-lambda @5.8-8.2
			(args
				(p-assign @5.9-5.13 (ident "pair")))
			(e-block @5.15-8.2
				(s-expr @6.5-6.22
					(e-tuple @6.5-6.20
						(elems
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-runtime-error (tag "ident_not_in_scope")))))
				(s-expr @6.23-7.6
					(e-lookup-local @6.23-6.27
						(pattern @5.9-5.13)))
				(e-tuple @7.5-7.20
					(elems
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope"))))))
		(annotation @5.1-5.5
			(declared-type
				(ty-fn @4.8-4.24 (effectful false)
					(ty-tuple @4.8-4.14
						(ty-var @4.9-4.10 (name "a"))
						(ty-var @4.12-4.13 (name "b")))
					(ty-tuple @4.18-4.24
						(ty-var @4.19-4.20 (name "b"))
						(ty-var @4.22-4.23 (name "a")))))))
	(d-let
		(p-assign @10.1-10.6 (ident "main!"))
		(e-lambda @10.9-10.15
			(args
				(p-underscore @10.10-10.11))
			(e-empty_record @10.13-10.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "* -> (Error, Error)"))
		(patt @10.1-10.6 (type "* -> {}")))
	(expressions
		(expr @5.8-8.2 (type "* -> (Error, Error)"))
		(expr @10.9-10.15 (type "* -> {}"))))
~~~
