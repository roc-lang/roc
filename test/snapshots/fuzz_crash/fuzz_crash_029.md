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
(block
  (malformed malformed:expr_unexpected_token)
  (str_literal_small "..l")
  (malformed malformed:expr_unexpected_token)
  (lc "mmen")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (apply_uc
      (uc "Ok")
      (lc "world")
    )
    (lc "n")
  )
  (lc "ar")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
platform # Cd
"foo" # Ce
 requires {(
	 # d
n! : List Str => {  },
)} #Ce
 exposes  []

"..l"
mmen
# Cose
# Cd
[
	Ok(world),
	n # pen
,
]ar
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:4 to 6:4

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 7:2 to 7:2

**Parse Error**
at 1:1 to 8:3

**Expected Exposes**
at 1:1 to 8:3

**Parse Error**
at 1:1 to 11:3

**Parse Error**
at 12:3 to 12:3

**Expected Close Curly Brace**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:1

**Parse Error**
at 1:1 to 13:4

**Parse Error**
at 13:4 to 13:4

**Parse Error**
at 13:11 to 13:11

**Parse Error**
at 13:19 to 13:19

**Parse Error**
at 14:2 to 14:2

**Parse Error**
at 16:1 to 16:1

**Parse Error**
at 15:3 to 16:1

**Parse Error**
at 16:3 to 16:3

**Parse Error**
at 17:3 to 17:3

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
