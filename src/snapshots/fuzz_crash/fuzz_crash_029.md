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
# EXPECTED
PARSE ERROR - fuzz_crash_029.md:4:4:4:9
PARSE ERROR - fuzz_crash_029.md:5:14:5:18
PARSE ERROR - fuzz_crash_029.md:5:9:5:14
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_029.md:5:24:5:31
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_029.md:6:4:6:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_029.md:7:2:7:13
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_029.md:10:2:10:15
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_029.md:14:2:14:15
PARSE ERROR - fuzz_crash_029.md:17:3:17:4
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**PARSE ERROR**
A parsing error occurred: `expected_requires_rigids_close_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_029.md:4:4:4:9:**
```roc
			{ # d
```
   ^^^^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_029.md:5:14:5:18:**
```roc
			n! : List(Str) => {}, # ure
```
             ^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**fuzz_crash_029.md:5:9:5:14:**
```roc
			n! : List(Str) => {}, # ure
```
        ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ure** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_029.md:5:24:5:31:**
```roc
			n! : List(Str) => {}, # ure
```
                       ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} #Ce** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_029.md:6:4:6:10:**
```roc
			} #Ce
```
   ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposes #rd** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_029.md:7:2:7:13:**
```roc
	exposes #rd
```
 ^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **packages # Cd** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_029.md:10:2:10:15:**
```roc
	packages # Cd
```
 ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **provides # Cd** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_029.md:14:2:14:15:**
```roc
	provides # Cd
```
 ^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_029.md:17:3:17:4:**
```roc
		]
```
  ^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**fuzz_crash_029.md:17:4:17:4:**
```roc
		]
```
   


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
KwRequires(3:2-3:10),Newline(3:12-3:26),
OpenCurly(4:4-4:5),Newline(4:7-4:9),
LowerIdent(5:4-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:13),NoSpaceOpenRound(5:13-5:14),UpperIdent(5:14-5:17),CloseRound(5:17-5:18),OpFatArrow(5:19-5:21),OpenCurly(5:22-5:23),CloseCurly(5:23-5:24),Comma(5:24-5:25),Newline(5:27-5:31),
CloseCurly(6:4-6:5),Newline(6:7-6:10),
KwExposes(7:2-7:9),Newline(7:11-7:13),
OpenSquare(8:3-8:4),Newline(8:6-8:6),
CloseSquare(9:3-9:4),Newline(9:6-9:10),
KwPackages(10:2-10:10),Newline(10:12-10:15),
LowerIdent(11:3-11:8),Newline(11:10-11:13),
OpenCurly(12:3-12:4),Newline(12:6-12:10),
LowerIdent(13:1-13:4),OpColon(13:4-13:5),StringStart(13:6-13:7),StringPart(13:7-13:10),StringEnd(13:10-13:11),Comma(13:11-13:12),LowerIdent(13:13-13:17),CloseCurly(13:19-13:20),Newline(13:22-13:27),
KwProvides(14:2-14:10),Newline(14:12-14:15),
OpenSquare(15:3-15:4),UpperIdent(15:5-15:7),NoSpaceOpenRound(15:7-15:8),LowerIdent(15:8-15:13),CloseRound(15:13-15:14),Comma(15:14-15:15),OpenRound(15:16-15:17),LowerIdent(15:17-15:18),Newline(15:20-15:24),
LowerIdent(16:1-16:3),Comma(16:3-16:4),Newline(1:1-1:1),
CloseRound(17:3-17:4),EndOfFile(17:4-17:4),
~~~
# PARSE
~~~clojure
(file @1.1-17.4
	(malformed-header @4.4-5.13 (tag "expected_requires_rigids_close_curly"))
	(statements
		(s-malformed @5.9-5.23 (tag "expected_colon_after_type_annotation"))
		(e-record @5.22-5.24)
		(e-malformed @5.24-5.31 (reason "expr_unexpected_token"))
		(e-malformed @6.4-6.10 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.13 (reason "expr_unexpected_token"))
		(e-list @8.3-9.4)
		(e-malformed @10.2-10.15 (reason "expr_unexpected_token"))
		(e-ident @11.3-11.8 (raw "vides"))
		(e-record @12.3-13.20
			(field (field "pkg") (optional false)
				(e-string @13.6-13.11
					(e-string-part @13.7-13.10 (raw "..l"))))
			(field (field "mmen") (optional false)))
		(e-malformed @14.2-14.15 (reason "expr_unexpected_token"))
		(e-malformed @17.4-17.4 (reason "expected_expr_close_square_or_comma"))))
~~~
# FORMATTED
~~~roc
 # Co		{	} #ose
{} # ure
 # Ce
 # rd
[] # Cse
 # Cd
vides # Cd
{ # pen

	pkg: "..l",
	mmen
} # Cose
 # Cd

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
