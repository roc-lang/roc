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
(header-only)
~~~
# FORMATTED
~~~roc
platform "foo" # Comment after name requires {(Main)} (main! : List Str => {  }) exposes  [
	foo,
] packages {some_pkg, ("../some_pkg.roc")}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:3 to 6:3

**Parse Error**
at 1:1 to 7:4

**Parse Error**
at 9:4 to 9:4

**Parse Error**
at 1:1 to 10:2

**Parse Error**
at 17:3 to 17:3

**Expected Close Curly Brace**
at 1:1 to 18:2

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
