# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ #
		] # Cse
	packages # Cd
		vides # Cd
		{ # pen
pkg: "..l", mmen		} # Cose
	provides # Cd
		[ Ok(world), (n # pen
ar,
		]
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma CloseCurly KwExposes OpenSquare CloseSquare KwPackages LowerIdent OpenCurly LowerIdent OpColon String Comma LowerIdent CloseCurly KwProvides OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent LowerIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(platform-header)
~~~
# FORMATTED
~~~roc
platform "foo" requires {(
	n! : List Str => {  },
)} exposes #rd
		 exposes  []

: "..l", mmen
} # Cose
	provides # Cd
		[
	Ok(world),
	n,
]ar,
		]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:4 to 7:2

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 7:2 to 8:3

**Parse Error**
at 1:1 to 8:3

**Expected Exposes**
at 1:1 to 8:3

**Parse Error**
at 1:1 to 11:3

**Parse Error**
at 12:3 to 13:1

**Expected Close Curly Brace**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:4

**Parse Error**
at 13:4 to 13:6

**Parse Error**
at 13:11 to 13:13

**Parse Error**
at 13:19 to 14:2

**Parse Error**
at 14:2 to 15:3

**Parse Error**
at 16:1 to 16:1

**Parse Error**
at 15:3 to 16:1

**Parse Error**
at 16:3 to 17:3

**Parse Error**
at 17:3 to 17:4

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.lookup "mmen")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.list_literal)
  (Expr.lookup "ar")
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
