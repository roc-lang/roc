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
KwPlatform LineComment String LineComment KwRequires OpenCurly CloseCurly LineComment OpenCurly LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpFatArrow OpenCurly CloseCurly Comma LineComment CloseCurly LineComment KwExposes LineComment OpenSquare Dot CloseSquare LineComment KwPackages LineComment OpenCurly LineComment LowerIdent OpColon Int String Comma LowerIdent CloseCurly LineComment KwProvides LineComment OpenSquare LineComment LowerIdent Comma CloseSquare ~~~
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
  (list_literal
    (malformed)
  )
  (malformed)
  (record_literal
    (binop_colon
      (lc "pkg")
      (num_literal_i32 77)
    )
    (str_literal_small "..c")
    (binop_colon
      (lc "mm")
      (lc "mm")
    )
  )
  (malformed)
  (list_literal
    (lc "ar")
  )
)
~~~
# FORMATTED
~~~roc
platform # Cd
"foo" requires # Ce
#ose
{} exposes [: ]

Str
) 
=> 
{}
, # ure
			
} #Ce
	
exposes #rd
		
[.
		]
# Cse
packages # Cd
		
{ # pen
	pkg: 77,
	"..c",
	mm: mm,
}
#
provides # Cd
		
[ # pen
	ar,
]
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
**PARSE ERROR**
A parsing error occurred: **expected_colon_after_pat_field_name**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:5:6:5:8:**
```roc
			{n! : List(Str) => {}, # ure
```
			  ^^


**PARSE ERROR**
A parsing error occurred: **expected_requires_signatures_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:8:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**EXPECTED EXPOSES**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_030.md:1:1:5:8:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**EXPECTED OPENING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_030.md:1:1:5:8:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:5:8:5:10:**
```roc
			{n! : List(Str) => {}, # ure
```
			    ^^


**EXPECTED CLOSING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_030.md:1:1:5:10:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**EXPECTED PACKAGES**
A parsing error occurred: **expected_packages**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:10:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_packages_open_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:10:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **List** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:5:10:5:14:**
```roc
			{n! : List(Str) => {}, # ure
```
			      ^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_packages_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:14:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_provides**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:14:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **expected_provides_open_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:14:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:5:14:5:15:**
```roc
			{n! : List(Str) => {}, # ure
```
			          ^


**PARSE ERROR**
A parsing error occurred: **expected_provides_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_030.md:1:1:5:15:**
```roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:5:18:5:20:**
```roc
			{n! : List(Str) => {}, # ure
```
			              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:5:20:5:23:**
```roc
			{n! : List(Str) => {}, # ure
```
			                ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ure
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:5:25:6:4:**
```roc
			{n! : List(Str) => {}, # ure
			} #Ce
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} #Ce
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:6:4:7:2:**
```roc
			} #Ce
	exposes #rd
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposes #rd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:7:2:8:3:**
```roc
	exposes #rd
		[ .
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:8:5:9:3:**
```roc
		[ .
		] # Cse
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **packages # Cd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:10:2:11:3:**
```roc
	packages # Cd
		{ # pen
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **provides # Cd
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_030.md:13:2:14:3:**
```roc
	provides # Cd
		[ # pen
```


**UNDEFINED VARIABLE**
Nothing is named **mm** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_030.md:12:15:12:17:**
```roc
pkg: 77"..c", mm} #
```
              ^^


**UNDEFINED VARIABLE**
Nothing is named **ar** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_030.md:15:1:15:3:**
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
  (Expr.record_literal
    (Expr.record_field
      (Expr.malformed)
      (Expr.num_literal_i32 77)
    )
    (Expr.str_literal_small)
    (Expr.record_field
      (Expr.lookup "mm")
      (Expr.lookup "mm")
    )
  )
  (Expr.malformed)
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 38
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
(var #10 -> #30)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 Num *)
(var #19 _)
(var #20 Str)
(var #21 _)
(var #22 _)
(var #23 -> #36)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 {})
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 {})
(var #36 record)
(var #37 _)
~~~
# TYPES
~~~roc
~~~
