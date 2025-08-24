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
  (uc "Str")
  (malformed malformed:expr_unexpected_token)
  (record_literal)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (malformed malformed:expr_unexpected_token)
  )
  (malformed malformed:expr_unexpected_token)
  (record_literal
    (binop_colon
      (lc "pkg")
      (num_literal_i32 77)
    )
    (str_literal_small "..c")
    (lc "mm")
  )
  (malformed malformed:expr_unexpected_token)
  (list_literal
    (tuple_literal
      (lc "ar")
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_030.md:8:5:8:6
PARSE ERROR - fuzz_crash_030.md:12:8:12:9
PARSE ERROR - fuzz_crash_030.md:12:9:12:12
PARSE ERROR - fuzz_crash_030.md:12:12:12:13
PARSE ERROR - fuzz_crash_030.md:12:13:12:14
PARSE ERROR - fuzz_crash_030.md:12:15:12:17
PARSE ERROR - fuzz_crash_030.md:12:17:12:18
PARSE ERROR - fuzz_crash_030.md:13:2:13:10
PARSE ERROR - fuzz_crash_030.md:14:3:14:4
PARSE ERROR - fuzz_crash_030.md:15:1:15:3
PARSE ERROR - fuzz_crash_030.md:15:3:15:4
PARSE ERROR - fuzz_crash_030.md:16:3:16:4
# PROBLEMS
**Parse Error**
at 1:1 to 5:6

**Expected Exposes**
at 1:1 to 5:6

**Expected Open Square Bracket**
at 1:1 to 5:6

**Parse Error**
at 5:6 to 5:6

**Expected Close Square Bracket**
at 1:1 to 5:8

**Expected Packages**
at 1:1 to 5:8

**Parse Error**
at 1:1 to 5:8

**Parse Error**
at 5:8 to 5:8

**Expected Close Curly Brace**
at 1:1 to 5:10

**Parse Error**
at 1:1 to 5:10

**Parse Error**
at 1:1 to 5:10

**Parse Error**
at 1:1 to 5:14

**Parse Error**
at 5:20 to 5:20

**Parse Error**
at 5:25 to 5:25

**Parse Error**
at 6:4 to 6:4

**Parse Error**
at 7:2 to 7:2

**Parse Error**
at 8:5 to 8:5

**Parse Error**
at 10:2 to 10:2

**Parse Error**
at 13:2 to 13:2

**Parse Error**
at 16:3 to 16:3

**Parse Error**
at 14:3 to 16:4

**Unsupported Node**
at 5:20 to 5:20

**Unsupported Node**
at 5:25 to 5:25

**Unsupported Node**
at 6:4 to 6:4

**Unsupported Node**
at 7:2 to 7:2

**Unsupported Node**
at 8:3 to 9:3

**Unsupported Node**
at 10:2 to 10:2

**Unsupported Node**
at 13:2 to 13:2

**Unsupported Node**
at 14:3 to 16:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "pkg")
      (Expr.num_literal_i32 77)
    )
    (Expr.str_literal_small)
    (Expr.lookup "mm")
  )
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
