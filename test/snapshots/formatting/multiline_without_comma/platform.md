# META
~~~ini
description=Multiline without comma formatting platform
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires {
		R1,
		R2
	} {
		r1 : R1 -> R2,
		r2 : R1 -> R2
	}
	exposes [
		E1,
		E2
	]
	packages {
		pa1: "pa1",
		pa2: "pa2"
	}
	# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
	provides [
		pr1,
		pr2
	]
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly UpperIdent Comma UpperIdent CloseCurly OpenCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent Comma LowerIdent OpColon UpperIdent OpArrow UpperIdent CloseCurly KwExposes OpenSquare UpperIdent Comma UpperIdent CloseSquare KwPackages OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly KwProvides OpenSquare LowerIdent Comma LowerIdent CloseSquare ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
platform "pf" requires {(R1, R2)} (r1 : R1 -> R2, r2) : R1 -> R2 exposes  [E1, E2] packages {pa1, ("pa1", pa2) : "pa2"}

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
