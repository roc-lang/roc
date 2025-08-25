# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ .
		] # Cse
	packages # Cd
		{ # pen
pkg: 77"..c", mm} #
	provides # Cd
		[ # pen
ar,
		]
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma CloseCurly KwExposes OpenSquare Dot CloseSquare KwPackages OpenCurly LowerIdent OpColon Int String Comma LowerIdent CloseCurly KwProvides OpenSquare LowerIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (lc "ar")
  )
)
~~~
# FORMATTED
~~~roc
platform "foo" # Ce requires (
	n!: List(Str) => {  },
) exposes  [
	.
		] # Cse
	packages # Cd
		{ # pen,
] packages {pkg, 77}

} #
provides # Cd
[
	ar,
]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:4 to 6:4

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 8:5 to 8:5

**Expected Close Curly Brace**
at 1:1 to 12:8

**Parse Error**
at 1:1 to 12:8

**Parse Error**
at 1:1 to 12:8

**Parse Error**
at 12:8 to 12:8

**Parse Error**
at 1:1 to 12:17

**Parse Error**
at 12:17 to 12:17

**Parse Error**
at 13:2 to 13:2

**Unsupported Node**
at 12:17 to 12:17

**Unsupported Node**
at 13:2 to 13:2

**Unsupported Node**
at 14:3 to 16:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
