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
KwPlatform LineComment String LineComment KwRequires LineComment OpenCurly LineComment UpperIdent Comma LineComment CloseCurly LineComment OpenCurly LineComment LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma LineComment CloseCurly LineComment KwExposes LineComment OpenSquare LineComment LowerIdent Comma LineComment CloseSquare LineComment KwPackages LineComment OpenCurly LineComment LowerIdent OpColon String Comma LineComment CloseCurly LineComment KwProvides LineComment OpenSquare LineComment LowerIdent Comma LineComment CloseSquare ~~~
# PARSE
~~~clojure
(platform-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
platform "foo" requires {Main} {
} exposes [
	: ,
]

Str
) 
=> 
{}
, # Comment after signature
			
} # Comment after signatures close
	
exposes # Comment after exposes keyword
		
[
	foo,
]
packages # Comment after packages keyword
		
{
	some_pkg : "../some_pkg.roc",
}
provides # Comment after provides keyword
		
[
	bar,
]# Comment after provides open
# Comment after exposed item
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_colon_after_pat_field_name**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:8:9:8:11:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				    ^^


**PARSE ERROR**
A parsing error occurred: **expected_requires_signatures_close_curly**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:11:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**EXPECTED EXPOSES**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**platform_header_nonempty_1.md:1:1:8:11:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**EXPECTED OPENING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**platform_header_nonempty_1.md:1:1:8:11:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:8:11:8:13:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				      ^^


**EXPECTED CLOSING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**platform_header_nonempty_1.md:1:1:8:13:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**EXPECTED PACKAGES**
A parsing error occurred: **expected_packages**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:13:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**PARSE ERROR**
A parsing error occurred: **expected_packages_open_curly**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:13:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **List** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:8:13:8:17:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				        ^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_packages_close_curly**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:17:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**PARSE ERROR**
A parsing error occurred: **expected_provides**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:17:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**PARSE ERROR**
A parsing error occurred: **expected_provides_open_square**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:17:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:8:17:8:18:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				            ^


**PARSE ERROR**
A parsing error occurred: **expected_provides_close_square**
This is an unexpected parsing error. Please check your syntax.

**platform_header_nonempty_1.md:1:1:8:18:**
```roc
platform # Comment after platform keyword
	"foo" # Comment after name
	requires # Comment after requires keyword
		{ # Comment after rigids open
			Main, # Comment after rigid member
		} # Comment after rigids close
			{ # Comment after signatures open
				main! : List(Str) => {}, # Comment after signature
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:8:21:8:23:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				                ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:8:23:8:26:**
```roc
				main! : List(Str) => {}, # Comment after signature
```
				                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment after signature
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:8:28:9:4:**
```roc
				main! : List(Str) => {}, # Comment after signature
			} # Comment after signatures close
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Comment after signatures close
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:9:4:10:2:**
```roc
			} # Comment after signatures close
	exposes # Comment after exposes keyword
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposes # Comment after exposes keyword
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:10:2:11:3:**
```roc
	exposes # Comment after exposes keyword
		[ # Comment after exposes open
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **packages # Comment after packages keyword
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:14:2:15:3:**
```roc
	packages # Comment after packages keyword
		{ # Comment after packages open
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **provides # Comment after provides keyword
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**platform_header_nonempty_1.md:18:2:19:3:**
```roc
	provides # Comment after provides keyword
		[ # Comment after provides open
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "some_pkg")
      (Expr.str_literal_big)
    )
  )
  (Expr.malformed)
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
