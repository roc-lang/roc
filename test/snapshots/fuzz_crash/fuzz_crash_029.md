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
**PARSE ERROR**
A parsing error occurred: `expected_requires_rigids_close_curly`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:4:4:4:5:**
```roc
			{ # d
```
			^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:14:5:17:**
```roc
			n! : List(Str) => {}, # ure
```
			          ^^^


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

**fuzz_crash_029.md:5:19:5:21:**
```roc
			n! : List(Str) => {}, # ure
```
			               ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:22:5:23:**
```roc
			n! : List(Str) => {}, # ure
```
			                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:23:5:24:**
```roc
			n! : List(Str) => {}, # ure
```
			                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:5:24:5:25:**
```roc
			n! : List(Str) => {}, # ure
```
			                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:6:4:6:5:**
```roc
			} #Ce
```
			^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:7:2:7:9:**
```roc
	exposes #rd
```
	^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:8:3:8:4:**
```roc
		[ #
```
		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:9:3:9:4:**
```roc
		] # Cse
```
		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:10:2:10:10:**
```roc
	packages # Cd
```
	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:11:3:11:8:**
```roc
		vides # Cd
```
		^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:12:3:12:4:**
```roc
		{ # pen
```
		^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_029.md:13:6:13:7:**
```roc
pkg: "..l", mmen		} # Cose
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:7:13:10:**
```roc
pkg: "..l", mmen		} # Cose
```
      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:10:13:11:**
```roc
pkg: "..l", mmen		} # Cose
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:11:13:12:**
```roc
pkg: "..l", mmen		} # Cose
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:13:13:17:**
```roc
pkg: "..l", mmen		} # Cose
```
            ^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:13:19:13:20:**
```roc
pkg: "..l", mmen		} # Cose
```
                		^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:14:2:14:10:**
```roc
	provides # Cd
```
	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:3:15:4:**
```roc
		[ Ok(world), (n # pen
```
		^


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

**fuzz_crash_029.md:15:14:15:15:**
```roc
		[ Ok(world), (n # pen
```
		           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:16:15:17:**
```roc
		[ Ok(world), (n # pen
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:15:17:15:18:**
```roc
		[ Ok(world), (n # pen
```
		              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:16:1:16:3:**
```roc
ar,
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:16:3:16:4:**
```roc
ar,
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_029.md:17:3:17:4:**
```roc
		]
```
		^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_029.md:13:6:13:7:**
```roc
pkg: "..l", mmen		} # Cose
```
     ^


# TOKENS
~~~zig
KwPlatform(1:1-1:9),
StringStart(2:2-2:3),StringPart(2:3-2:6),StringEnd(2:6-2:7),
KwRequires(3:2-3:10),
OpenCurly(4:4-4:5),
LowerIdent(5:4-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:13),NoSpaceOpenRound(5:13-5:14),UpperIdent(5:14-5:17),CloseRound(5:17-5:18),OpFatArrow(5:19-5:21),OpenCurly(5:22-5:23),CloseCurly(5:23-5:24),Comma(5:24-5:25),
CloseCurly(6:4-6:5),
KwExposes(7:2-7:9),
OpenSquare(8:3-8:4),
CloseSquare(9:3-9:4),
KwPackages(10:2-10:10),
LowerIdent(11:3-11:8),
OpenCurly(12:3-12:4),
LowerIdent(13:1-13:4),OpColon(13:4-13:5),StringStart(13:6-13:7),StringPart(13:7-13:10),StringEnd(13:10-13:11),Comma(13:11-13:12),LowerIdent(13:13-13:17),CloseCurly(13:19-13:20),
KwProvides(14:2-14:10),
OpenSquare(15:3-15:4),UpperIdent(15:5-15:7),NoSpaceOpenRound(15:7-15:8),LowerIdent(15:8-15:13),CloseRound(15:13-15:14),Comma(15:14-15:15),OpenRound(15:16-15:17),LowerIdent(15:17-15:18),
LowerIdent(16:1-16:3),Comma(16:3-16:4),
CloseSquare(17:3-17:4),
EndOfFile(18:1-18:1),
~~~
# PARSE
~~~clojure
(file @1.1-17.4
	(malformed-header @4.4-5.8 (tag "expected_requires_rigids_close_curly"))
	(statements
		(s-malformed @5.19-5.21 (tag "expected_colon_after_type_annotation"))
		(s-malformed @5.22-5.23 (tag "statement_unexpected_token"))
		(s-malformed @5.23-5.24 (tag "statement_unexpected_token"))
		(s-malformed @5.24-5.25 (tag "statement_unexpected_token"))
		(s-malformed @6.4-6.5 (tag "statement_unexpected_token"))
		(s-malformed @7.2-7.9 (tag "statement_unexpected_token"))
		(s-malformed @8.3-8.4 (tag "statement_unexpected_token"))
		(s-malformed @9.3-9.4 (tag "statement_unexpected_token"))
		(s-malformed @10.2-10.10 (tag "statement_unexpected_token"))
		(s-malformed @11.3-11.8 (tag "statement_unexpected_token"))
		(s-malformed @12.3-12.4 (tag "statement_unexpected_token"))
		(s-type-anno @13.1-13.7 (name "pkg")
			(ty-malformed @13.6-13.7 (tag "ty_anno_unexpected_token")))
		(s-malformed @13.7-13.10 (tag "statement_unexpected_token"))
		(s-malformed @13.10-13.11 (tag "statement_unexpected_token"))
		(s-malformed @13.11-13.12 (tag "statement_unexpected_token"))
		(s-malformed @13.13-13.17 (tag "statement_unexpected_token"))
		(s-malformed @13.19-13.20 (tag "statement_unexpected_token"))
		(s-malformed @14.2-14.10 (tag "statement_unexpected_token"))
		(s-malformed @15.3-15.4 (tag "statement_unexpected_token"))
		(s-malformed @15.14-15.15 (tag "expected_colon_after_type_annotation"))
		(s-malformed @15.16-15.17 (tag "statement_unexpected_token"))
		(s-malformed @15.17-15.18 (tag "statement_unexpected_token"))
		(s-malformed @16.1-16.3 (tag "statement_unexpected_token"))
		(s-malformed @16.3-16.4 (tag "statement_unexpected_token"))
		(s-malformed @17.3-17.4 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# ure
# Ce
# rd
#
# Cse
# Cd
# Cd
# pen
pkg : 
# Cose
# Cd
# pen

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
