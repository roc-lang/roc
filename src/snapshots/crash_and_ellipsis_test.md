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

main! = |_|
    result1 = testEllipsis(42)
    result2 = testCrash(42)
    result3 = testCrashSimple(42)
    []
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash_and_ellipsis_test.md:9:17:9:24:**
```roc
testCrash = |_| crash "This is a crash message"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **crash "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash_and_ellipsis_test.md:13:23:13:30:**
```roc
testCrashSimple = |_| crash "oops"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= testEllipsis** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash_and_ellipsis_test.md:16:13:16:27:**
```roc
    result1 = testEllipsis(42)
```


**NOT IMPLEMENTED**
This feature is not yet implemented: ...

**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNDEFINED VARIABLE**
Nothing is named `result1` in this scope.
Is there an `import` or `exposing` missing up-top?

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

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
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),Newline(1:1-1:1),
LowerIdent(16:5-16:12),OpAssign(16:13-16:14),LowerIdent(16:15-16:27),NoSpaceOpenRound(16:27-16:28),Int(16:28-16:30),CloseRound(16:30-16:31),Newline(1:1-1:1),
LowerIdent(17:5-17:12),OpAssign(17:13-17:14),LowerIdent(17:15-17:24),NoSpaceOpenRound(17:24-17:25),Int(17:25-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
LowerIdent(18:5-18:12),OpAssign(18:13-18:14),LowerIdent(18:15-18:30),NoSpaceOpenRound(18:30-18:31),Int(18:31-18:33),CloseRound(18:33-18:34),Newline(1:1-1:1),
OpenSquare(19:5-19:6),CloseSquare(19:6-19:7),EndOfFile(19:7-19:7),
~~~
# PARSE
~~~clojure
(file @1-1-19-7
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
		(s-type-anno @4-1-5-13 (name "testEllipsis")
			(ty-fn @4-16-4-26
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl @5-1-5-23
			(p-ident @5-1-5-13 (raw "testEllipsis"))
			(e-lambda @5-16-5-23
				(args
					(p-underscore))
				(e-ellipsis)))
		(s-type-anno @8-1-9-10 (name "testCrash")
			(ty-fn @8-13-8-23
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl @9-1-9-24
			(p-ident @9-1-9-10 (raw "testCrash"))
			(e-lambda @9-13-9-24
				(args
					(p-underscore))
				(e-malformed @9-17-9-24 (reason "expr_unexpected_token"))))
		(e-string @9-23-9-48
			(e-string-part @9-24-9-47 (raw "This is a crash message")))
		(s-type-anno @12-1-13-16 (name "testCrashSimple")
			(ty-fn @12-19-12-29
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl @13-1-13-30
			(p-ident @13-1-13-16 (raw "testCrashSimple"))
			(e-lambda @13-19-13-30
				(args
					(p-underscore))
				(e-malformed @13-23-13-30 (reason "expr_unexpected_token"))))
		(e-string @13-29-13-35
			(e-string-part @13-30-13-34 (raw "oops")))
		(s-decl @15-1-16-12
			(p-ident @15-1-15-6 (raw "main!"))
			(e-lambda @15-9-16-12
				(args
					(p-underscore))
				(e-ident @16-5-16-12 (qaul "") (raw "result1"))))
		(e-malformed @16-13-16-27 (reason "expr_unexpected_token"))
		(e-apply @16-15-16-31
			(e-ident @16-15-16-27 (qaul "") (raw "testEllipsis"))
			(e-int @16-28-16-30 (raw "42")))
		(s-decl @17-5-17-28
			(p-ident @17-5-17-12 (raw "result2"))
			(e-apply @17-15-17-28
				(e-ident @17-15-17-24 (qaul "") (raw "testCrash"))
				(e-int @17-25-17-27 (raw "42"))))
		(s-decl @18-5-18-34
			(p-ident @18-5-18-12 (raw "result3"))
			(e-apply @18-15-18-34
				(e-ident @18-15-18-30 (qaul "") (raw "testCrashSimple"))
				(e-int @18-31-18-33 (raw "42"))))
		(e-list @19-5-19-7)))
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

main! = |_|
	result1testEllipsis(42)
result2 = testCrash(42)
result3 = testCrashSimple(42)
[]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 85)
		(p-assign @5-1-5-13 (ident "testEllipsis") (id 75))
		(e-lambda @5-16-5-23 (id 79)
			(args
				(p-underscore @5-17-5-18 (id 76)))
			(e-runtime-error (tag "not_implemented")))
		(annotation @5-1-5-13 (signature 83) (id 84)
			(declared-type
				(ty-fn @4-16-4-26 (effectful false)
					(ty @4-16-4-19 (name "U64"))
					(ty @4-23-4-26 (name "U64"))))))
	(d-let (id 99)
		(p-assign @9-1-9-10 (ident "testCrash") (id 89))
		(e-lambda @9-13-9-24 (id 93)
			(args
				(p-underscore @9-14-9-15 (id 90)))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @9-1-9-10 (signature 97) (id 98)
			(declared-type
				(ty-fn @8-13-8-23 (effectful false)
					(ty @8-13-8-16 (name "U64"))
					(ty @8-20-8-23 (name "U64"))))))
	(d-let (id 114)
		(p-assign @13-1-13-16 (ident "testCrashSimple") (id 104))
		(e-lambda @13-19-13-30 (id 108)
			(args
				(p-underscore @13-20-13-21 (id 105)))
			(e-runtime-error (tag "lambda_body_not_canonicalized")))
		(annotation @13-1-13-16 (signature 112) (id 113)
			(declared-type
				(ty-fn @12-19-12-29 (effectful false)
					(ty @12-19-12-22 (name "U64"))
					(ty @12-26-12-29 (name "U64"))))))
	(d-let (id 121)
		(p-assign @15-1-15-6 (ident "main!") (id 116))
		(e-lambda @15-9-16-12 (id 120)
			(args
				(p-underscore @15-10-15-11 (id 117)))
			(e-runtime-error (tag "ident_not_in_scope"))))
	(d-let (id 130)
		(p-assign @17-5-17-12 (ident "result2") (id 124))
		(e-call @17-15-17-28 (id 129)
			(e-lookup-local @17-15-17-24
				(pattern (id 89)))
			(e-int @17-25-17-27 (num-var 128) (sign-needed "false") (bits-needed "7") (value "42"))))
	(d-let (id 137)
		(p-assign @18-5-18-12 (ident "result3") (id 131))
		(e-call @18-15-18-34 (id 136)
			(e-lookup-local @18-15-18-30
				(pattern (id 104)))
			(e-int @18-31-18-33 (num-var 135) (sign-needed "false") (bits-needed "7") (value "42")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "testEllipsis") (type "*"))
		(def (name "testCrash") (type "*"))
		(def (name "testCrashSimple") (type "*"))
		(def (name "main!") (type "*"))
		(def (name "result2") (type "*"))
		(def (name "result3") (type "*")))
	(expressions
		(expr @5-16-5-23 (type "*"))
		(expr @9-13-9-24 (type "*"))
		(expr @13-19-13-30 (type "*"))
		(expr @15-9-16-12 (type "*"))
		(expr @17-15-17-28 (type "*"))
		(expr @18-15-18-34 (type "*"))))
~~~