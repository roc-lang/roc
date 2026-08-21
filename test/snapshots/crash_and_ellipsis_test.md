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
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 20 5) (end 20 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result1")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result1")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "crash_and_ellipsis_test.md") (start 20 5) (end 20 12) (annotation error) (line-text "    result1 = testEllipsis(42)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 21 5) (end 21 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result2")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result2")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "crash_and_ellipsis_test.md") (start 21 5) (end 21 12) (annotation error) (line-text "    result2 = testCrash(42)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 22 5) (end 22 12))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "result3")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_result3")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "crash_and_ellipsis_test.md") (start 22 5) (end 22 12) (annotation error) (line-text "    result3 = testCrashSimple(42)")))))
~~~
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
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "testCrash"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-string
							(e-literal (string "This is a crash message")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "testCrashSimple"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-run-low-level (op "crash")
					(args
						(e-string
							(e-literal (string "oops")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "result1"))
					(e-call (constraint-fn-var 303)
						(e-lookup-local
							(p-assign (ident "testEllipsis")))
						(e-num (value "42"))))
				(s-let
					(p-assign (ident "result2"))
					(e-call (constraint-fn-var 311)
						(e-lookup-local
							(p-assign (ident "testCrash")))
						(e-num (value "42"))))
				(s-let
					(p-assign (ident "result3"))
					(e-call (constraint-fn-var 319)
						(e-lookup-local
							(p-assign (ident "testCrashSimple")))
						(e-num (value "42"))))
				(e-empty_list)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> U64"))
		(patt (type "U64 -> U64"))
		(patt (type "U64 -> U64"))
		(patt (type "_arg -> List(_a)")))
	(expressions
		(expr (type "U64 -> U64"))
		(expr (type "U64 -> U64"))
		(expr (type "U64 -> U64"))
		(expr (type "_arg -> List(_a)"))))
~~~
