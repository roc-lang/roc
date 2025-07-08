# META
~~~ini
description=Test crash and ellipsis canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Test ellipsis placeholder
testEllipsis : U64 -> U64
testEllipsis = |_| ...

# Test crash statement
testCrash : U64 -> U64
testCrash = |_| crash "This is a crash message"

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| crash "oops"

main! = |_| {
    result1 = testEllipsis(42)
    result2 = testCrash(42)
    result3 = testCrashSimple(42)
    []
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - crash_and_ellipsis_test.md:9:17:9:24
UNEXPECTED TOKEN IN EXPRESSION - crash_and_ellipsis_test.md:13:23:13:30
INVALID STATEMENT - crash_and_ellipsis_test.md:1:1:1:1
INVALID STATEMENT - crash_and_ellipsis_test.md:1:1:1:1
UNUSED VARIABLE - crash_and_ellipsis_test.md:16:5:16:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:17:5:17:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:18:5:18:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash_and_ellipsis_test.md:9:17:9:24:**
```roc
testCrash = |_| crash "This is a crash message"
```
                ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash_and_ellipsis_test.md:13:23:13:30:**
```roc
testCrashSimple = |_| crash "oops"
```
                      ^^^^^^^


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**crash_and_ellipsis_test.md:1:1:1:1:**
```roc

```



**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**crash_and_ellipsis_test.md:1:1:1:1:**
```roc

```



**UNUSED VARIABLE**
Variable ``result1`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:16:5:16:12:**
```roc
    result1 = testEllipsis(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable ``result2`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:17:5:17:12:**
```roc
    result2 = testCrash(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable ``result3`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:18:5:18:12:**
```roc
    result3 = testCrashSimple(42)
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:28),
LowerIdent(4:1-4:13),OpColon(4:14-4:15),UpperIdent(4:16-4:19),OpArrow(4:20-4:22),UpperIdent(4:23-4:26),Newline(1:1-1:1),
LowerIdent(5:1-5:13),OpAssign(5:14-5:15),OpBar(5:16-5:17),Underscore(5:17-5:18),OpBar(5:18-5:19),TripleDot(5:20-5:23),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:23),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:16),OpArrow(8:17-8:19),UpperIdent(8:20-8:23),Newline(1:1-1:1),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),Underscore(9:14-9:15),OpBar(9:15-9:16),KwCrash(9:17-9:22),StringStart(9:23-9:24),StringPart(9:24-9:47),StringEnd(9:47-9:48),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:36),
LowerIdent(12:1-12:16),OpColon(12:17-12:18),UpperIdent(12:19-12:22),OpArrow(12:23-12:25),UpperIdent(12:26-12:29),Newline(1:1-1:1),
LowerIdent(13:1-13:16),OpAssign(13:17-13:18),OpBar(13:19-13:20),Underscore(13:20-13:21),OpBar(13:21-13:22),KwCrash(13:23-13:28),StringStart(13:29-13:30),StringPart(13:30-13:34),StringEnd(13:34-13:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),Newline(1:1-1:1),
LowerIdent(16:5-16:12),OpAssign(16:13-16:14),LowerIdent(16:15-16:27),NoSpaceOpenRound(16:27-16:28),Int(16:28-16:30),CloseRound(16:30-16:31),Newline(1:1-1:1),
LowerIdent(17:5-17:12),OpAssign(17:13-17:14),LowerIdent(17:15-17:24),NoSpaceOpenRound(17:24-17:25),Int(17:25-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
LowerIdent(18:5-18:12),OpAssign(18:13-18:14),LowerIdent(18:15-18:30),NoSpaceOpenRound(18:30-18:31),Int(18:31-18:33),CloseRound(18:33-18:34),Newline(1:1-1:1),
OpenSquare(19:5-19:6),CloseSquare(19:6-19:7),Newline(1:1-1:1),
CloseCurly(20:1-20:2),EndOfFile(20:2-20:2),
~~~
# PARSE
~~~clojure
(file @1.1-20.2
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
		(s-type-anno @1.1-1.1 (name "testEllipsis")
			(ty-fn @4.16-4.26
				(ty @4.16-4.19 (name "U64"))
				(ty @4.23-4.26 (name "U64"))))
		(s-decl @5.1-5.23
			(p-ident @5.1-5.13 (raw "testEllipsis"))
			(e-lambda @5.16-5.23
				(args
					(p-underscore))
				(e-ellipsis)))
		(s-type-anno @1.1-1.1 (name "testCrash")
			(ty-fn @8.13-8.23
				(ty @8.13-8.16 (name "U64"))
				(ty @8.20-8.23 (name "U64"))))
		(s-decl @9.1-9.24
			(p-ident @9.1-9.10 (raw "testCrash"))
			(e-lambda @9.13-9.24
				(args
					(p-underscore))
				(e-malformed @9.17-9.24 (reason "expr_unexpected_token"))))
		(e-string @9.23-9.48
			(e-string-part @9.24-9.47 (raw "This is a crash message")))
		(s-type-anno @1.1-1.1 (name "testCrashSimple")
			(ty-fn @12.19-12.29
				(ty @12.19-12.22 (name "U64"))
				(ty @12.26-12.29 (name "U64"))))
		(s-decl @13.1-13.30
			(p-ident @13.1-13.16 (raw "testCrashSimple"))
			(e-lambda @13.19-13.30
				(args
					(p-underscore))
				(e-malformed @13.23-13.30 (reason "expr_unexpected_token"))))
		(e-string @13.29-13.35
			(e-string-part @13.30-13.34 (raw "oops")))
		(s-decl @15.1-20.2
			(p-ident @15.1-15.6 (raw "main!"))
			(e-lambda @15.9-20.2
				(args
					(p-underscore))
				(e-block @15.13-20.2
					(statements
						(s-decl @16.5-16.31
							(p-ident @16.5-16.12 (raw "result1"))
							(e-apply @16.15-16.31
								(e-ident @16.15-16.27 (raw "testEllipsis"))
								(e-int @16.28-16.30 (raw "42"))))
						(s-decl @17.5-17.28
							(p-ident @17.5-17.12 (raw "result2"))
							(e-apply @17.15-17.28
								(e-ident @17.15-17.24 (raw "testCrash"))
								(e-int @17.25-17.27 (raw "42"))))
						(s-decl @18.5-18.34
							(p-ident @18.5-18.12 (raw "result3"))
							(e-apply @18.15-18.34
								(e-ident @18.15-18.30 (raw "testCrashSimple"))
								(e-int @18.31-18.33 (raw "42"))))
						(e-list @19.5-19.7)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Test ellipsis placeholder
testEllipsis : U64 -> U64
testEllipsis = |_| ...

# Test crash statement
testCrash : U64 -> U64
testCrash = |_| "This is a crash message"

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| "oops"

main! = |_| {
	result1 = testEllipsis(42)
	result2 = testCrash(42)
	result3 = testCrashSimple(42)
	[]
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.13 (ident "testEllipsis"))
		(e-lambda @5.16-5.23
			(args
				(p-underscore @5.17-5.18))
			(e-not-implemented))
		(annotation @5.1-5.13
			(declared-type
				(ty-fn @4.16-4.26 (effectful false)
					(ty @4.16-4.19 (name "U64"))
					(ty @4.23-4.26 (name "U64"))))))
	(d-let
		(p-assign @9.1-9.10 (ident "testCrash"))
		(e-lambda @9.13-9.24
			(args
				(p-underscore @9.14-9.15))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.23 (effectful false)
					(ty @8.13-8.16 (name "U64"))
					(ty @8.20-8.23 (name "U64"))))))
	(d-let
		(p-assign @13.1-13.16 (ident "testCrashSimple"))
		(e-lambda @13.19-13.30
			(args
				(p-underscore @13.20-13.21))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @13.1-13.16
			(declared-type
				(ty-fn @12.19-12.29 (effectful false)
					(ty @12.19-12.22 (name "U64"))
					(ty @12.26-12.29 (name "U64"))))))
	(d-let
		(p-assign @15.1-15.6 (ident "main!"))
		(e-lambda @15.9-20.2
			(args
				(p-underscore @15.10-15.11))
			(e-block @15.13-20.2
				(s-let @16.5-16.31
					(p-assign @16.5-16.12 (ident "result1"))
					(e-call @16.15-16.31
						(e-lookup-local @16.15-16.27
							(pattern @5.1-5.13))
						(e-int @16.28-16.30 (value "42"))))
				(s-let @17.5-17.28
					(p-assign @17.5-17.12 (ident "result2"))
					(e-call @17.15-17.28
						(e-lookup-local @17.15-17.24
							(pattern @9.1-9.10))
						(e-int @17.25-17.27 (value "42"))))
				(s-let @18.5-18.34
					(p-assign @18.5-18.12 (ident "result3"))
					(e-call @18.15-18.34
						(e-lookup-local @18.15-18.30
							(pattern @13.1-13.16))
						(e-int @18.31-18.33 (value "42"))))
				(e-empty_list @19.5-19.7)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.13 (type "U64 -> U64"))
		(patt @9.1-9.10 (type "U64 -> Error"))
		(patt @13.1-13.16 (type "U64 -> Error"))
		(patt @15.1-15.6 (type "* -> List(*)")))
	(expressions
		(expr @5.16-5.23 (type "U64 -> U64"))
		(expr @9.13-9.24 (type "U64 -> Error"))
		(expr @13.19-13.30 (type "U64 -> Error"))
		(expr @15.9-20.2 (type "* -> List(*)"))))
~~~
