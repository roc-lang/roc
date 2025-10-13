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
testCrash = |_| {
	crash "This is a crash message"
}

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| {
	crash "oops"
}

main! = |_| {
    result1 = testEllipsis(42)
    result2 = testCrash(42)
    result3 = testCrashSimple(42)
    []
}
~~~
# EXPECTED
UNUSED VARIABLE - crash_and_ellipsis_test.md:20:5:20:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:21:5:21:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:22:5:22:12
# PROBLEMS
**UNUSED VARIABLE**
Variable `result1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:20:5:20:12:**
```roc
    result1 = testEllipsis(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:21:5:21:12:**
```roc
    result2 = testCrash(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**crash_and_ellipsis_test.md:22:5:22:12:**
```roc
    result3 = testCrashSimple(42)
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
LowerIdent(4:1-4:13),OpColon(4:14-4:15),UpperIdent(4:16-4:19),OpArrow(4:20-4:22),UpperIdent(4:23-4:26),
LowerIdent(5:1-5:13),OpAssign(5:14-5:15),OpBar(5:16-5:17),Underscore(5:17-5:18),OpBar(5:18-5:19),TripleDot(5:20-5:23),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:16),OpArrow(8:17-8:19),UpperIdent(8:20-8:23),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),Underscore(9:14-9:15),OpBar(9:15-9:16),OpenCurly(9:17-9:18),
KwCrash(10:2-10:7),StringStart(10:8-10:9),StringPart(10:9-10:32),StringEnd(10:32-10:33),
CloseCurly(11:1-11:2),
LowerIdent(14:1-14:16),OpColon(14:17-14:18),UpperIdent(14:19-14:22),OpArrow(14:23-14:25),UpperIdent(14:26-14:29),
LowerIdent(15:1-15:16),OpAssign(15:17-15:18),OpBar(15:19-15:20),Underscore(15:20-15:21),OpBar(15:21-15:22),OpenCurly(15:23-15:24),
KwCrash(16:2-16:7),StringStart(16:8-16:9),StringPart(16:9-16:13),StringEnd(16:13-16:14),
CloseCurly(17:1-17:2),
LowerIdent(19:1-19:6),OpAssign(19:7-19:8),OpBar(19:9-19:10),Underscore(19:10-19:11),OpBar(19:11-19:12),OpenCurly(19:13-19:14),
LowerIdent(20:5-20:12),OpAssign(20:13-20:14),LowerIdent(20:15-20:27),NoSpaceOpenRound(20:27-20:28),Int(20:28-20:30),CloseRound(20:30-20:31),
LowerIdent(21:5-21:12),OpAssign(21:13-21:14),LowerIdent(21:15-21:24),NoSpaceOpenRound(21:24-21:25),Int(21:25-21:27),CloseRound(21:27-21:28),
LowerIdent(22:5-22:12),OpAssign(22:13-22:14),LowerIdent(22:15-22:30),NoSpaceOpenRound(22:30-22:31),Int(22:31-22:33),CloseRound(22:33-22:34),
OpenSquare(23:5-23:6),CloseSquare(23:6-23:7),
CloseCurly(24:1-24:2),
EndOfFile(25:1-25:1),
~~~
# PARSE
~~~clojure
(file @1.1-24.2
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.26 (name "testEllipsis")
			(ty-fn @4.16-4.26
				(ty @4.16-4.19 (name "U64"))
				(ty @4.23-4.26 (name "U64"))))
		(s-decl @5.1-5.23
			(p-ident @5.1-5.13 (raw "testEllipsis"))
			(e-lambda @5.16-5.23
				(args
					(p-underscore))
				(e-ellipsis)))
		(s-type-anno @8.1-8.23 (name "testCrash")
			(ty-fn @8.13-8.23
				(ty @8.13-8.16 (name "U64"))
				(ty @8.20-8.23 (name "U64"))))
		(s-decl @9.1-11.2
			(p-ident @9.1-9.10 (raw "testCrash"))
			(e-lambda @9.13-11.2
				(args
					(p-underscore))
				(e-block @9.17-11.2
					(statements
						(s-crash @10.2-10.33
							(e-string @10.8-10.33
								(e-string-part @10.9-10.32 (raw "This is a crash message"))))))))
		(s-type-anno @14.1-14.29 (name "testCrashSimple")
			(ty-fn @14.19-14.29
				(ty @14.19-14.22 (name "U64"))
				(ty @14.26-14.29 (name "U64"))))
		(s-decl @15.1-17.2
			(p-ident @15.1-15.16 (raw "testCrashSimple"))
			(e-lambda @15.19-17.2
				(args
					(p-underscore))
				(e-block @15.23-17.2
					(statements
						(s-crash @16.2-16.14
							(e-string @16.8-16.14
								(e-string-part @16.9-16.13 (raw "oops"))))))))
		(s-decl @19.1-24.2
			(p-ident @19.1-19.6 (raw "main!"))
			(e-lambda @19.9-24.2
				(args
					(p-underscore))
				(e-block @19.13-24.2
					(statements
						(s-decl @20.5-20.31
							(p-ident @20.5-20.12 (raw "result1"))
							(e-apply @20.15-20.31
								(e-ident @20.15-20.27 (raw "testEllipsis"))
								(e-int @20.28-20.30 (raw "42"))))
						(s-decl @21.5-21.28
							(p-ident @21.5-21.12 (raw "result2"))
							(e-apply @21.15-21.28
								(e-ident @21.15-21.24 (raw "testCrash"))
								(e-int @21.25-21.27 (raw "42"))))
						(s-decl @22.5-22.34
							(p-ident @22.5-22.12 (raw "result3"))
							(e-apply @22.15-22.34
								(e-ident @22.15-22.30 (raw "testCrashSimple"))
								(e-int @22.31-22.33 (raw "42"))))
						(e-list @23.5-23.7)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Test ellipsis placeholder
testEllipsis : U64 -> U64
testEllipsis = |_| ...

# Test crash statement
testCrash : U64 -> U64
testCrash = |_| {
	crash "This is a crash message"
}

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| {
	crash "oops"
}

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
			(e-not-implemented @1.1-1.1))
		(annotation @5.1-5.13
			(declared-type
				(ty-fn @4.16-4.26 (effectful false)
					(ty-lookup @4.16-4.19 (name "U64") (builtin))
					(ty-lookup @4.23-4.26 (name "U64") (builtin))))))
	(d-let
		(p-assign @9.1-9.10 (ident "testCrash"))
		(e-lambda @9.13-11.2
			(args
				(p-underscore @9.14-9.15))
			(e-block @9.17-11.2
				(e-crash @10.2-10.33 (msg "This is a crash message"))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.23 (effectful false)
					(ty-lookup @8.13-8.16 (name "U64") (builtin))
					(ty-lookup @8.20-8.23 (name "U64") (builtin))))))
	(d-let
		(p-assign @15.1-15.16 (ident "testCrashSimple"))
		(e-lambda @15.19-17.2
			(args
				(p-underscore @15.20-15.21))
			(e-block @15.23-17.2
				(e-crash @16.2-16.14 (msg "oops"))))
		(annotation @15.1-15.16
			(declared-type
				(ty-fn @14.19-14.29 (effectful false)
					(ty-lookup @14.19-14.22 (name "U64") (builtin))
					(ty-lookup @14.26-14.29 (name "U64") (builtin))))))
	(d-let
		(p-assign @19.1-19.6 (ident "main!"))
		(e-closure @19.9-24.2
			(captures
				(capture @9.1-9.10 (ident "testCrash"))
				(capture @5.1-5.13 (ident "testEllipsis"))
				(capture @15.1-15.16 (ident "testCrashSimple")))
			(e-lambda @19.9-24.2
				(args
					(p-underscore @19.10-19.11))
				(e-block @19.13-24.2
					(s-let @20.5-20.31
						(p-assign @20.5-20.12 (ident "result1"))
						(e-call @20.15-20.31
							(e-lookup-local @20.15-20.27
								(p-assign @5.1-5.13 (ident "testEllipsis")))
							(e-num @20.28-20.30 (value "42"))))
					(s-let @21.5-21.28
						(p-assign @21.5-21.12 (ident "result2"))
						(e-call @21.15-21.28
							(e-lookup-local @21.15-21.24
								(p-assign @9.1-9.10 (ident "testCrash")))
							(e-num @21.25-21.27 (value "42"))))
					(s-let @22.5-22.34
						(p-assign @22.5-22.12 (ident "result3"))
						(e-call @22.15-22.34
							(e-lookup-local @22.15-22.30
								(p-assign @15.1-15.16 (ident "testCrashSimple")))
							(e-num @22.31-22.33 (value "42"))))
					(e-empty_list @23.5-23.7))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.13 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt @9.1-9.10 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt @15.1-15.16 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt @19.1-19.6 (type "_arg -> List(_elem)")))
	(expressions
		(expr @5.16-5.23 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr @9.13-11.2 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr @15.19-17.2 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr @19.9-24.2 (type "_arg -> List(_elem)"))))
~~~
