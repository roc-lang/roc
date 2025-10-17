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
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwCrash,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwCrash,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
OpenSquare,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "testEllipsis")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "testEllipsis"))
			(e-lambda
				(args
					(p-underscore))
				(e-ellipsis)))
		(s-type-anno (name "testCrash")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "testCrash"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-crash
							(e-string
								(e-string-part (raw "This is a crash message"))))))))
		(s-type-anno (name "testCrashSimple")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "testCrashSimple"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-crash
							(e-string
								(e-string-part (raw "oops"))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result1"))
							(e-apply
								(e-ident (raw "testEllipsis"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "testCrash"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "testCrashSimple"))
								(e-int (raw "42"))))
						(e-list)))))))
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
		(p-assign (ident "testEllipsis"))
		(e-lambda
			(args
				(p-underscore))
			(e-not-implemented))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "testCrash"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-crash (msg "This is a crash message"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "testCrashSimple"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-crash (msg "oops"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "testEllipsis"))
				(capture (ident "testCrash"))
				(capture (ident "testCrashSimple")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result1"))
						(e-call
							(e-lookup-local
								(p-assign (ident "testEllipsis")))
							(e-num (value "42"))))
					(s-let
						(p-assign (ident "result2"))
						(e-call
							(e-lookup-local
								(p-assign (ident "testCrash")))
							(e-num (value "42"))))
					(s-let
						(p-assign (ident "result3"))
						(e-call
							(e-lookup-local
								(p-assign (ident "testCrashSimple")))
							(e-num (value "42"))))
					(e-empty_list))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "_arg -> List(_elem)")))
	(expressions
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "_arg -> List(_elem)"))))
~~~
