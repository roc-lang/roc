# META
~~~ini
description=platform_header_nonempty (1)
type=file
~~~
# SOURCE
~~~roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
			} # Comment after signatures close
	exposes # Comment after exposes keyword
		[ # Comment after exposes open
			foo, # Comment after exposed item
		] # Comment after exposes close
	packages # Comment after packages keyword
		{ # Comment after packages open
			some_pkg: "../some_pkg.roc", # Comment after package
		} # Comment after packages close
	provides # Comment after provides keyword
		[ # Comment after provides open
			bar, # Comment after exposed item
		]
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly UpperIdent Comma CloseCurly OpenCurly LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma CloseCurly KwExposes OpenSquare LowerIdent Comma CloseSquare KwPackages OpenCurly LowerIdent OpColon String Comma CloseCurly KwProvides OpenSquare LowerIdent Comma CloseSquare ~~~
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
    (tuple_literal
      (lc "foo")
      (malformed malformed:expr_unexpected_token)
    )
  )
  (malformed malformed:expr_unexpected_token)
  (block
    (binop_colon
      (lc "some_pkg")
      (tuple_literal
        (str_literal_big "../some_pkg.roc")
        (malformed malformed:expr_unexpected_token)
      )
    )
    (malformed malformed:expr_unexpected_token)
    (list_literal
      (tuple_literal
        (lc "bar")
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_header_nonempty_1.md:12:4:12:7
# PROBLEMS
**Parse Error**
at 6:3 to 6:3

**Parse Error**
at 1:1 to 7:4

**Parse Error**
at 1:1 to 8:9

**Expected Exposes**
at 1:1 to 8:9

**Expected Open Square Bracket**
at 1:1 to 8:9

**Parse Error**
at 8:9 to 8:9

**Expected Close Square Bracket**
at 1:1 to 8:11

**Expected Packages**
at 1:1 to 8:11

**Parse Error**
at 1:1 to 8:11

**Parse Error**
at 8:11 to 8:11

**Expected Close Curly Brace**
at 1:1 to 8:13

**Parse Error**
at 1:1 to 8:13

**Parse Error**
at 1:1 to 8:13

**Parse Error**
at 1:1 to 8:17

**Parse Error**
at 8:23 to 8:23

**Parse Error**
at 8:28 to 8:28

**Parse Error**
at 9:4 to 9:4

**Parse Error**
at 10:2 to 10:2

**Parse Error**
at 13:3 to 13:3

**Parse Error**
at 11:3 to 14:2

**Parse Error**
at 14:2 to 14:2

**Parse Error**
at 17:3 to 17:3

**Parse Error**
at 18:2 to 18:2

**Parse Error**
at 21:3 to 21:3

**Parse Error**
at 19:3 to 21:4

**Parse Error**
at 15:3 to 21:4

**Pattern in Expression Context**
at 8:18 to 8:21

**Unsupported Node**
at 8:23 to 8:23

**Unsupported Node**
at 8:28 to 8:28

**Unsupported Node**
at 9:4 to 9:4

**Unsupported Node**
at 10:2 to 10:2

**Unsupported Node**
at 11:3 to 13:5

**Unsupported Node**
at 14:2 to 14:2

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 18:2 to 18:2

**Unsupported Node**
at 19:3 to 21:3

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
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "some_pkg")
      (Expr.malformed)
    )
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
