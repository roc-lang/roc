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
KwPlatform String KwRequires OpenCurly UpperIdent Comma UpperIdent Comma CloseCurly OpenCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent Comma LowerIdent OpColon UpperIdent OpArrow UpperIdent Comma CloseCurly KwExposes OpenSquare UpperIdent Comma UpperIdent Comma CloseSquare KwPackages OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly LineComment KwProvides OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(platform-header
  (exposes
    (uc "E1")

    (uc "E2")
)
  (packages
    (lc "pa1")

    (binop_colon
      (tuple_literal
        (str_literal_small "pa1")
        (lc "pa2")
      )
      (str_literal_small "pa2")
    )
))
~~~
# FORMATTED
~~~roc
platform "pf" requires {R1, R2} { r1: R1 -> R2, r2: R1 -> R2 } exposes [
	E1,
	E2,
] packages {pa1, ("pa1", pa2) : "pa2"}# imports [I1.{ I11, I12, }, I2.{ I21, I22, },]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:3:11:3:13
EXPOSED BUT NOT DEFINED - platform.md:3:15:3:17
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
