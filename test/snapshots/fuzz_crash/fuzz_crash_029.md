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
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
platform "foo" requires {} exposes [
	: ,
]

Str
) 
=> 
{}
, # ure
			
} #Ce
	
exposes #rd
		
[,
]
packages # Cd
		
vides
{
	pkg : "..l",
	mmen,
}
provides # Cd
		
[
	Ok(world),
	n,
]
ar
,
		
]
~~~
# EXPECTED
NIL
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
  (Expr.lookup "vides")
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "pkg")
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
