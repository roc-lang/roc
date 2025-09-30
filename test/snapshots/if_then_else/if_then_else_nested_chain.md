# META
~~~ini
description=Nested if-then-else chain demonstrating flattening
type=file
~~~
# SOURCE
~~~roc
checkNumber = |num| {
	if num < 0 {
		"negative"
	} else if num == 0 {
		"zero"
	} else if num > 100 {
		"large"
	} else {
		"positive"
	}
}
~~~
# EXPECTED
MISSING MAIN! FUNCTION - if_then_else_nested_chain.md:1:1:11:2
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**if_then_else_nested_chain.md:1:1:11:2:**
```roc
checkNumber = |num| {
	if num < 0 {
		"negative"
	} else if num == 0 {
		"zero"
	} else if num > 100 {
		"large"
	} else {
		"positive"
	}
}
```


# TOKENS
~~~zig
LowerIdent(1:1-1:12),OpAssign(1:13-1:14),OpBar(1:15-1:16),LowerIdent(1:16-1:19),OpBar(1:19-1:20),OpenCurly(1:21-1:22),
KwIf(2:2-2:4),LowerIdent(2:5-2:8),OpLessThan(2:9-2:10),Int(2:11-2:12),OpenCurly(2:13-2:14),
StringStart(3:3-3:4),StringPart(3:4-3:12),StringEnd(3:12-3:13),
CloseCurly(4:2-4:3),KwElse(4:4-4:8),KwIf(4:9-4:11),LowerIdent(4:12-4:15),OpEquals(4:16-4:18),Int(4:19-4:20),OpenCurly(4:21-4:22),
StringStart(5:3-5:4),StringPart(5:4-5:8),StringEnd(5:8-5:9),
CloseCurly(6:2-6:3),KwElse(6:4-6:8),KwIf(6:9-6:11),LowerIdent(6:12-6:15),OpGreaterThan(6:16-6:17),Int(6:18-6:21),OpenCurly(6:22-6:23),
StringStart(7:3-7:4),StringPart(7:4-7:9),StringEnd(7:9-7:10),
CloseCurly(8:2-8:3),KwElse(8:4-8:8),OpenCurly(8:9-8:10),
StringStart(9:3-9:4),StringPart(9:4-9:12),StringEnd(9:12-9:13),
CloseCurly(10:2-10:3),
CloseCurly(11:1-11:2),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.2
	(type-module @1.1-1.12)
	(statements
		(s-decl @1.1-11.2
			(p-ident @1.1-1.12 (raw "checkNumber"))
			(e-lambda @1.15-11.2
				(args
					(p-ident @1.16-1.19 (raw "num")))
				(e-block @1.21-11.2
					(statements
						(e-if-then-else @2.2-10.3
							(e-binop @2.5-2.12 (op "<")
								(e-ident @2.5-2.8 (raw "num"))
								(e-int @2.11-2.12 (raw "0")))
							(e-block @2.13-4.3
								(statements
									(e-string @3.3-3.13
										(e-string-part @3.4-3.12 (raw "negative")))))
							(e-if-then-else @4.9-10.3
								(e-binop @4.12-4.20 (op "==")
									(e-ident @4.12-4.15 (raw "num"))
									(e-int @4.19-4.20 (raw "0")))
								(e-block @4.21-6.3
									(statements
										(e-string @5.3-5.9
											(e-string-part @5.4-5.8 (raw "zero")))))
								(e-if-then-else @6.9-10.3
									(e-binop @6.12-6.21 (op ">")
										(e-ident @6.12-6.15 (raw "num"))
										(e-int @6.18-6.21 (raw "100")))
									(e-block @6.22-8.3
										(statements
											(e-string @7.3-7.10
												(e-string-part @7.4-7.9 (raw "large")))))
									(e-block @8.9-10.3
										(statements
											(e-string @9.3-9.13
												(e-string-part @9.4-9.12 (raw "positive"))))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.12 (ident "checkNumber"))
		(e-lambda @1.15-11.2
			(args
				(p-assign @1.16-1.19 (ident "num")))
			(e-block @1.21-11.2
				(e-if @2.2-10.3
					(if-branches
						(if-branch
							(e-binop @2.5-2.12 (op "lt")
								(e-lookup-local @2.5-2.8
									(p-assign @1.16-1.19 (ident "num")))
								(e-int @2.11-2.12 (value "0")))
							(e-block @2.13-4.3
								(e-string @3.3-3.13
									(e-literal @3.4-3.12 (string "negative")))))
						(if-branch
							(e-binop @4.12-4.20 (op "eq")
								(e-lookup-local @4.12-4.15
									(p-assign @1.16-1.19 (ident "num")))
								(e-int @4.19-4.20 (value "0")))
							(e-block @4.21-6.3
								(e-string @5.3-5.9
									(e-literal @5.4-5.8 (string "zero")))))
						(if-branch
							(e-binop @6.12-6.21 (op "gt")
								(e-lookup-local @6.12-6.15
									(p-assign @1.16-1.19 (ident "num")))
								(e-int @6.18-6.21 (value "100")))
							(e-block @6.22-8.3
								(e-string @7.3-7.10
									(e-literal @7.4-7.9 (string "large"))))))
					(if-else
						(e-block @8.9-10.3
							(e-string @9.3-9.13
								(e-literal @9.4-9.12 (string "positive"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.12 (type "Num(_size) -> Str")))
	(expressions
		(expr @1.15-11.2 (type "Num(_size) -> Str"))))
~~~
