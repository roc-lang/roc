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
			
} #Ce
	
exposes #rd
		
[.
		]
packages # Cd
		
{
	pkg : 77,
	"..c",
	mm,
}
provides # Cd
		
[
	ar,
]# pen
~~~
# EXPECTED
NIL
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
      (Expr.lookup "pkg")
      (Expr.num_literal_i32 77)
    )
    (Expr.str_literal_small)
    (Expr.lookup "mm")
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
