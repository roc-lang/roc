# META
~~~ini
description=Singleline with comma formatting platform
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires { R1, R2, } { r1 : R1 -> R2, r2 : R1 -> R2, }
	exposes [E1, E2,]
	packages { pa1: "pa1", pa2: "pa2", }
	# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
	provides [pr1, pr2,]
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly UpperIdent Comma UpperIdent Comma CloseCurly OpenCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent Comma LowerIdent OpColon UpperIdent OpArrow UpperIdent Comma CloseCurly KwExposes OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwPackages OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly KwProvides OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
platform "pf" requires {(
	R1,
	R2,
)} (
	(
		r1 : R1 -> R2,
		r2,
	) : R1 -> R2,
) exposes  [
	E1,
	E2,
] packages {pa1, (
	(
		"pa1",
		pa2,
	) : "pa2",
)}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 1:1 to 2:23

**Parse Error**
at 2:55 to 2:55

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 4:37 to 4:37

**Expected Close Curly Brace**
at 1:1 to 6:2

# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No top-level expression found in file
~~~
