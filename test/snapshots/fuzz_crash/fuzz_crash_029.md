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
  (malformed malformed:expr_unexpected_token)
  (record_literal)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (list_literal)
  (malformed malformed:expr_unexpected_token)
  (lc "vides")
  (block
    (binop_colon
      (lc "pkg")
      (tuple_literal
        (str_literal_small "..l")
        (lc "mmen")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (tuple_literal
      (apply_uc
        (uc "Ok")
        (lc "world")
      )
      (lc "n")
    )
  )
  (lc "ar")
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_029.md:4:4:4:5
PARSE ERROR - fuzz_crash_029.md:5:14:5:17
PARSE ERROR - fuzz_crash_029.md:5:19:5:21
PARSE ERROR - fuzz_crash_029.md:5:22:5:23
PARSE ERROR - fuzz_crash_029.md:5:23:5:24
PARSE ERROR - fuzz_crash_029.md:5:24:5:25
PARSE ERROR - fuzz_crash_029.md:6:4:6:5
PARSE ERROR - fuzz_crash_029.md:7:2:7:9
PARSE ERROR - fuzz_crash_029.md:8:3:8:4
PARSE ERROR - fuzz_crash_029.md:9:3:9:4
PARSE ERROR - fuzz_crash_029.md:10:2:10:10
PARSE ERROR - fuzz_crash_029.md:11:3:11:8
PARSE ERROR - fuzz_crash_029.md:12:3:12:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_029.md:13:6:13:7
PARSE ERROR - fuzz_crash_029.md:13:7:13:10
PARSE ERROR - fuzz_crash_029.md:13:10:13:11
PARSE ERROR - fuzz_crash_029.md:13:11:13:12
PARSE ERROR - fuzz_crash_029.md:13:13:13:17
PARSE ERROR - fuzz_crash_029.md:13:19:13:20
PARSE ERROR - fuzz_crash_029.md:14:2:14:10
PARSE ERROR - fuzz_crash_029.md:15:3:15:4
PARSE ERROR - fuzz_crash_029.md:15:14:15:15
PARSE ERROR - fuzz_crash_029.md:15:16:15:17
PARSE ERROR - fuzz_crash_029.md:15:17:15:18
PARSE ERROR - fuzz_crash_029.md:16:1:16:3
PARSE ERROR - fuzz_crash_029.md:16:3:16:4
PARSE ERROR - fuzz_crash_029.md:17:3:17:4
MALFORMED TYPE - fuzz_crash_029.md:13:6:13:7
# PROBLEMS
**Parse Error**
at 1:1 to 5:5

**Parse Error**
at 1:1 to 5:5

**Parse Error**
at 5:7 to 5:7

**Parse Error**
at 1:1 to 5:9

**Expected Exposes**
at 1:1 to 5:9

**Expected Open Square Bracket**
at 1:1 to 5:9

**Expected Close Square Bracket**
at 1:1 to 5:13

**Expected Packages**
at 1:1 to 5:13

**Parse Error**
at 1:1 to 5:13

**Parse Error**
at 5:13 to 5:13

**Expected Close Curly Brace**
at 1:1 to 5:14

**Parse Error**
at 1:1 to 5:14

**Parse Error**
at 1:1 to 5:14

**Parse Error**
at 1:1 to 5:17

**Parse Error**
at 5:17 to 5:17

**Parse Error**
at 5:19 to 5:19

**Parse Error**
at 5:24 to 5:24

**Parse Error**
at 6:4 to 6:4

**Parse Error**
at 7:2 to 7:2

**Parse Error**
at 10:2 to 10:2

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

**Unsupported Node**
at 5:17 to 5:17

**Unsupported Node**
at 5:19 to 5:19

**Unsupported Node**
at 5:24 to 5:24

**Unsupported Node**
at 6:4 to 6:4

**Unsupported Node**
at 7:2 to 7:2

**Unsupported Node**
at 8:3 to 8:4

**Unsupported Node**
at 10:2 to 10:2

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 14:2 to 14:2

**Unsupported Node**
at 15:3 to 15:20

**Unsupported Node**
at 16:3 to 16:3

**Unsupported Node**
at 17:3 to 17:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "vides")
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "pkg")
      (Expr.malformed)
    )
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "ar")
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
