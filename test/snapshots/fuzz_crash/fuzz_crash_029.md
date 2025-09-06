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
KwPlatform LineComment String LineComment KwRequires LineComment OpenCurly LineComment LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma LineComment CloseCurly LineComment KwExposes LineComment OpenSquare LineComment CloseSquare LineComment KwPackages LineComment LowerIdent LineComment OpenCurly LineComment LowerIdent OpColon String Comma LowerIdent CloseCurly LineComment KwProvides LineComment OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent LineComment LowerIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(platform-header
  (exposes
    (malformed)
))
(block
  (uc "Str")
  (malformed)
  (malformed)
  (record_literal)
  (malformed)
  (malformed)
  (malformed)
  (list_literal)
  (malformed)
  (lc "vides")
  (record_literal
    (binop_colon
      (lc "pkg")
      (str_literal_small "..l")
    )
    (lc "mmen")
  )
  (malformed)
  (list_literal
    (apply_uc
      (uc "Ok")
      (lc "world")
    )
    (lc "n")
  )
  (lc "ar")
  (malformed)
  (malformed)
)
~~~
# FORMATTED
~~~roc
platform # Cd
"foo" requires # Ce
# Co		{	} #ose
# d
{} exposes [: ]

Str
) 
=> 
{}
, # ure
			
} #Ce
	
exposes #rd
		
[, #
]
# Cse
packages # Cd
		
vides
# Cd
{ # pen
	pkg: "..l",
	mmen,
}
# Cose
provides # Cd
		
[
	Ok(world),
	n, # pen
]
ar
,
		
]
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
**UNEXPECTED TOKEN IN EXPRESSION**
The token **n** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:4:5:5:**
```roc
			n! : List(Str) => {}, # ure
```
			^


**PARSE ERROR**
A parsing error occurred: **expected_requires_rigids_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:5:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_requires_signatures_open_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:5:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **! ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:5:5:7:**
```roc
			n! : List(Str) => {}, # ure
```
			 ^^


**PARSE ERROR**
A parsing error occurred: **expected_requires_signatures_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:7:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**EXPECTED EXPOSES**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_029.md:1:1:5:7:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**EXPECTED OPENING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_029.md:1:1:5:7:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:7:5:9:**
```roc
			n! : List(Str) => {}, # ure
```
			   ^^


**EXPECTED CLOSING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_029.md:1:1:5:9:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**EXPECTED PACKAGES**
A parsing error occurred: **expected_packages**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:9:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_packages_open_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:9:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **List** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:9:5:13:**
```roc
			n! : List(Str) => {}, # ure
```
			     ^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_packages_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:13:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_provides**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:13:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_provides_open_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:13:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:13:5:14:**
```roc
			n! : List(Str) => {}, # ure
```
			         ^


**PARSE ERROR**
A parsing error occurred: **expected_provides_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:1:1:5:14:**
```roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:17:5:19:**
```roc
			n! : List(Str) => {}, # ure
```
			             ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:19:5:22:**
```roc
			n! : List(Str) => {}, # ure
```
			               ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ure
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:5:24:6:4:**
```roc
			n! : List(Str) => {}, # ure
			} #Ce
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} #Ce
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:6:4:7:2:**
```roc
			} #Ce
	exposes #rd
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposes #rd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:7:2:8:3:**
```roc
	exposes #rd
		[ #
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **packages # Cd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:10:2:11:3:**
```roc
	packages # Cd
		vides # Cd
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **provides # Cd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:14:2:15:3:**
```roc
	provides # Cd
		[ Ok(world), (n # pen
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_029.md:15:3:16:1:**
```roc
		[ Ok(world), (n # pen
ar,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:16:3:17:3:**
```roc
ar,
		]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_029.md:17:3:17:4:**
```roc
		]
```
		^


**UNDEFINED VARIABLE**
Nothing is named **vides** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_029.md:11:3:11:8:**
```roc
		vides # Cd
```
		^^^^^


**UNDEFINED VARIABLE**
Nothing is named **mmen** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_029.md:13:13:13:17:**
```roc
pkg: "..l", mmen		} # Cose
```
            ^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_029.md:15:8:15:13:**
```roc
		[ Ok(world), (n # pen
```
		     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **n** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_029.md:15:17:15:18:**
```roc
		[ Ok(world), (n # pen
```
		              ^


**UNDEFINED VARIABLE**
Nothing is named **ar** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_029.md:16:1:16:3:**
```roc
ar,
```
^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tag_no_args)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.record_literal
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.lookup "vides")
  (Expr.record_literal
    (Expr.record_field
      (Expr.malformed)
      (Expr.str_literal_small)
    )
    (Expr.lookup "mmen")
  )
  (Expr.malformed)
  (Expr.list_literal)
  (Expr.lookup "ar")
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 45
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 -> #35)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 Str)
(var #20 _)
(var #21 _)
(var #22 -> #41)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 {})
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 {})
(var #41 record)
(var #42 _)
(var #43 _)
(var #44 _)
~~~
# TYPES
~~~roc
~~~
