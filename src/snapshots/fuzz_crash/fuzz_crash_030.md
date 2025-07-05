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
# EXPECTED
PARSE ERROR - fuzz_crash_030.md:8:5:8:5
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:9:3:9:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:10:2:10:15
PARSE ERROR - fuzz_crash_030.md:12:8:12:12
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:12:9:12:13
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:12:12:12:14
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:12:13:12:17
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:12:17:12:20
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_030.md:13:2:13:15
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_030.md:8:5:8:5:**
```roc
		[ .
```
    


**EXPECTED CLOSING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

Here is the problematic code:
**fuzz_crash_030.md:8:3:8:6:**
```roc
		[ .
```
  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] # Cse** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:9:3:9:10:**
```roc
		] # Cse
```
  ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **packages # Cd** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:10:2:10:15:**
```roc
	packages # Cd
```
 ^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_030.md:12:8:12:12:**
```roc
pkg: 77"..c", mm} #
```
       ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **..c"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:12:9:12:13:**
```roc
pkg: 77"..c", mm} #
```
        ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:12:12:12:14:**
```roc
pkg: 77"..c", mm} #
```
           ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, mm** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:12:13:12:17:**
```roc
pkg: 77"..c", mm} #
```
            ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} #** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:12:17:12:20:**
```roc
pkg: 77"..c", mm} #
```
                ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **provides # Cd** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_030.md:13:2:13:15:**
```roc
	provides # Cd
```
 ^^^^^^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwPlatform(1:1-1:9),Newline(1:11-1:14),
StringStart(2:2-2:3),StringPart(2:3-2:6),StringEnd(2:6-2:7),Newline(2:9-2:12),
KwRequires(3:2-3:10),Newline(1:1-1:1),
OpenCurly(4:3-4:4),CloseCurly(4:5-4:6),Newline(4:8-4:11),
OpenCurly(5:4-5:5),LowerIdent(5:5-5:7),OpColon(5:8-5:9),UpperIdent(5:10-5:14),NoSpaceOpenRound(5:14-5:15),UpperIdent(5:15-5:18),CloseRound(5:18-5:19),OpFatArrow(5:20-5:22),OpenCurly(5:23-5:24),CloseCurly(5:24-5:25),Comma(5:25-5:26),Newline(5:28-5:32),
CloseCurly(6:4-6:5),Newline(6:7-6:10),
KwExposes(7:2-7:9),Newline(7:11-7:13),
OpenSquare(8:3-8:4),Dot(8:5-8:6),Newline(1:1-1:1),
CloseSquare(9:3-9:4),Newline(9:6-9:10),
KwPackages(10:2-10:10),Newline(10:12-10:15),
OpenCurly(11:3-11:4),Newline(11:6-11:10),
LowerIdent(12:1-12:4),OpColon(12:4-12:5),Int(12:6-12:8),StringStart(12:8-12:9),StringPart(12:9-12:12),StringEnd(12:12-12:13),Comma(12:13-12:14),LowerIdent(12:15-12:17),CloseCurly(12:17-12:18),Newline(12:20-12:20),
KwProvides(13:2-13:10),Newline(13:12-13:15),
OpenSquare(14:3-14:4),Newline(14:6-14:10),
LowerIdent(15:1-15:3),Comma(15:3-15:4),Newline(1:1-1:1),
CloseSquare(16:3-16:4),EndOfFile(16:4-16:4),
~~~
# PARSE
~~~clojure
(file @1.1-16.4
	(malformed-header @8.3-9.4 (tag "expected_exposes_close_square"))
	(statements
		(e-malformed @9.3-9.10 (reason "expr_unexpected_token"))
		(e-malformed @10.2-10.15 (reason "expr_unexpected_token"))
		(e-malformed @12.8-12.12 (reason "expected_expr_close_curly_or_comma"))
		(e-malformed @12.9-12.13 (reason "expr_unexpected_token"))
		(e-malformed @12.12-12.14 (reason "expr_unexpected_token"))
		(e-malformed @12.13-12.17 (reason "expr_unexpected_token"))
		(e-ident @12.15-12.17 (raw "mm"))
		(e-malformed @12.17-12.20 (reason "expr_unexpected_token"))
		(e-malformed @13.2-13.15 (reason "expr_unexpected_token"))
		(e-list @14.3-16.4
			(e-ident @15.1-15.3 (raw "ar")))))
~~~
# FORMATTED
~~~roc
 # rd

 # Cse
 # Cd
mm
 # Cd
[ # pen
	ar,
]
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
